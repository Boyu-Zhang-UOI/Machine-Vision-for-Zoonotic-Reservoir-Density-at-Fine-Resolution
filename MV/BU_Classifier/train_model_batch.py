import os, sys
# os.environ['CUDA_LAUNCH_BLOCKING'] = 1
from os.path import join
from glob import glob

import numpy as np
from PIL import Image
from matplotlib import pyplot as plt

import torch, torchvision
from torchvision.models.detection.faster_rcnn import FastRCNNPredictor
from torchvision.models.detection.mask_rcnn import MaskRCNNPredictor
from torchvision.models.detection.rpn import AnchorGenerator, RPNHead
import torch.utils.data
from natsort import natsorted

from engine import train_one_epoch, evaluate
import utils
import transforms as T
import random
import tqdm

num_epochs = 30
CUDA = torch.cuda.is_available()
if CUDA:
    print('GPU works')
    
DS_folder = '/mnt/ceph/boyuz/BU_Classifier/MRCNN/bantou_512'

# read image, mask (instance), and label based on the full path of image. 
def read_burned_raster(image_full_path):
    image = np.array(Image.open(image_full_path))
    mask = np.squeeze(np.load(image_full_path.replace('_image.png', '_mask.npy')))
    label = np.load(image_full_path.replace('_image.png', '_label.npy'))
    # validation 1 mask and image size
    # print(image.shape, mask.shape)
    if not np.array_equal(image.shape[:2], mask.shape):
        return False, image, mask, label
    # validation 2 labels and instances
    if len(np.unique(mask))-1 != len(label):
        return False, image, mask, label

    return True, image, mask, label

"""
The input of buildingDataset is the list of all images. The upper level program will create training and testing image lists based on towns (leave one town out). Then the training dataset and testing dataset will be created based on the list of images.
"""
class buildingDataset(torch.utils.data.Dataset):
    def __init__(self, fileList, transforms=None):
        self.images = natsorted(fileList)
        # self.masks = [x.replace('_image.png', '_mask.npy') for x in fileList]
        # self.labels = [x.replace('_image.png', '_label.npy') for x in fileList]
        self.transforms = transforms

    def __getitem__(self, idx):
        # load images ad masks
        img_path = self.images[idx]
        _, img, mask, lbl = read_burned_raster(img_path)

        # get seperate masks for each object
        obj_ids = np.unique(mask)[1:]
        mask_objs = mask==obj_ids[:, None, None]

        # get bboxes for objects
        num_objs = len(obj_ids)
        boxes = []
        for i in range(num_objs):
            pos = np.where(mask_objs[i])
            xmin = np.min(pos[1])
            xmax = np.max(pos[1])
            ymin = np.min(pos[0])
            ymax = np.max(pos[0])
            boxes.append([xmin, ymin, xmax, ymax])

        boxes = torch.as_tensor(boxes, dtype=torch.float32)
        area = (boxes[:, 3] - boxes[:, 1]) * (boxes[:, 2] - boxes[:, 0])
        
        lbl = torch.as_tensor(lbl, dtype=torch.int64)
        mask_objs = torch.as_tensor(mask_objs, dtype=torch.uint8)
        image_id = torch.tensor([idx])
        # no overlapping among houses
        iscrowd = torch.zeros((num_objs,), dtype=torch.int64)

        target = {}
        target['image_id'] = image_id
        target['labels'] = lbl
        target["boxes"] = boxes
        target["masks"] = mask_objs
        target["area"] = area
        target["iscrowd"] = iscrowd

        if self.transforms is not None:
            img, target = self.transforms(img, target)
            
        return img, target

    def __len__(self):
        return len(self.images)

def get_transform(train):
    transforms = []
    # converts the image, a PIL image, into a PyTorch Tensor
    transforms.append(T.ToTensor())
    if train:
        # during training, randomly flip the training images
        # and ground-truth for data augmentation
        transforms.append(T.RandomHorizontalFlip(0.5))
    return T.Compose(transforms)

def get_instance_segmentation_model(num_classes=3):
    # load an instance segmentation model pre-trained on COCO
    model = torchvision.models.detection.maskrcnn_resnet50_fpn(pretrained=True)

    # get the number of input features for the classifier
    in_features = model.roi_heads.box_predictor.cls_score.in_features
    # replace the pre-trained head with a new one
    model.roi_heads.box_predictor = FastRCNNPredictor(in_features, num_classes)
    anchor_generator = AnchorGenerator(
        sizes=tuple([(16, 32, 64, 128, 256) for _ in range(5)]),
        aspect_ratios=tuple([(0.5, 1, 2.0) for _ in range(5)]))
    model.rpn.anchor_generator = anchor_generator

    model.rpn.head = RPNHead(256, anchor_generator.num_anchors_per_location()[0])
    # now get the number of input features for the mask classifier
    in_features_mask = model.roi_heads.mask_predictor.conv5_mask.in_channels
    hidden_layer = 256
    # and replace the mask predictor with a new one
    model.roi_heads.mask_predictor = MaskRCNNPredictor(in_features_mask,
                                                       hidden_layer,
                                                       num_classes)

    return model

'''
we got 38 avaiable towns which means there are 4 towns without any qualified instances, besides the two with overlapping.
'''
# list tiles in the dataset
tile_list = glob(join(DS_folder, '*_image.png'))

# get list of avaiable towns
tile_name_list = [x.split('/')[-1] for x in tile_list]
town_name_list = [x.partition('_tile_')[0] for x in tile_name_list]
towns = np.unique(np.unique(town_name_list))
print(towns)

for testing_town in town_name_list:
    if testing_town in set(['Barlie', 'Benduma', 'Largo']):
        continue

    training_list = [x for x in tile_list if x.find(testing_town)==-1]
    testing_list = [x for x in tile_list if x.find(testing_town)!=-1]
    # print(testing_list)

    # create training and testing dataset
    ds_train = buildingDataset(training_list, get_transform(train=True))
    ds_test = buildingDataset(testing_list, get_transform(train=True))

    # define training and validation data loaders
    data_loader = torch.utils.data.DataLoader(
        ds_train, batch_size=1, shuffle=True, num_workers=1,
        collate_fn=utils.collate_fn)

    data_loader_test = torch.utils.data.DataLoader(
        ds_test, batch_size=1, shuffle=False, num_workers=1,
        collate_fn=utils.collate_fn)

    device = torch.device('cuda') if torch.cuda.is_available() else torch.device('cpu')

    # our dataset has two classes only - background and person
    num_classes = 3

    # get the model using our helper function
    model = get_instance_segmentation_model(num_classes)
    # parallel
    # model = torch.nn.DataParallel(model)
    # move model to the right device
    model.to(device)

    # construct an optimizer
    params = [p for p in model.parameters() if p.requires_grad]
    optimizer = torch.optim.SGD(params, lr=0.005,
                                momentum=0.9, weight_decay=0.0005)

    # and a learning rate scheduler which decreases the learning rate by
    # 10x every 3 epochs
    lr_scheduler = torch.optim.lr_scheduler.StepLR(optimizer,
                                                step_size=10,
                                                gamma=0.1)

    # let's train it for 30 epochs
    from torch.optim.lr_scheduler import StepLR

    for epoch in range(num_epochs):
        # train for one epoch, printing every 10 iterations
        train_one_epoch(model, optimizer, data_loader, device, epoch, print_freq=2000)
        # update the learning rate
        lr_scheduler.step()
        # evaluate on the test dataset
        # if epoch%10 == 1:
        #     evaluate(model, data_loader_test, device=device)

    # evaluate(model, data_loader_test, device=device)
    # save model to avoid more training
    PATH = '/mnt/ceph/boyuz/Builiding_Landscaping/trained_models/30_epoch_2048_' + testing_town
    torch.save(model, PATH)