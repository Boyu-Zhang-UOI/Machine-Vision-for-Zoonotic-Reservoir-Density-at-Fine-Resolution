# mask-rcnn utils
from operator import mod
from re import X
import numpy as np
from PIL import Image
import torch, torchvision
import transforms as T
from natsort import natsorted
import rasterio
from rasterio.warp import calculate_default_transform, reproject
from rasterio.enums import Resampling
from math import pi, cos, sin, radians
import os.path as path
import cv2
from copy import deepcopy, copy
import fiona
from shapely.geometry import mapping, Polygon

# Constants
PIXEL = 0.2204315

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

# read image, mask (instance), and label based on the full path of image. 
def read_burned_raster_npz(image_full_path):
    image = np.array(Image.open(image_full_path))
    mask = np.squeeze(np.load(image_full_path.replace('_image.png', '_mask.npz'))['label'])
    label = np.load(image_full_path.replace('_image.png', '_label.npy'))
    # validation 1 mask and image size
    # print(image.shape, mask.shape)
    if not np.array_equal(image.shape[:2], mask.shape):
        return False, image, mask, label
    # validation 2 labels and instances
    if len(np.unique(mask))-1 != len(label):
        return False, image, mask, label

    return True, image, mask, label


def get_iou(bb1, bb2):
    """
    Calculate the Intersection over Union (IoU) of two bounding boxes.

    Parameters
    ----------
    bb1 : dict
        Keys: {'x1', 'x2', 'y1', 'y2'}
        The (x1, y1) position is at the top left corner,
        the (x2, y2) position is at the bottom right corner
    bb2 : dict
        Keys: {'x1', 'x2', 'y1', 'y2'}
        The (x, y) position is at the top left corner,
        the (x2, y2) position is at the bottom right corner

    Returns
    -------
    float
        in [0, 1]
    """
    assert bb1['x1'] <= bb1['x2']
    assert bb1['y1'] <= bb1['y2']
    assert bb2['x1'] <= bb2['x2']
    assert bb2['y1'] <= bb2['y2']

    # determine the coordinates of the intersection rectangle
    x_left = max(bb1['x1'], bb2['x1'])
    y_top = max(bb1['y1'], bb2['y1'])
    x_right = min(bb1['x2'], bb2['x2'])
    y_bottom = min(bb1['y2'], bb2['y2'])

    if x_right < x_left or y_bottom < y_top:
        return 0.0

    # The intersection of two axis-aligned bounding boxes is always an
    # axis-aligned bounding box
    intersection_area = (x_right - x_left + 1) * (y_bottom - y_top + 1)

    # compute the area of both AABBs
    bb1_area = (bb1['x2'] - bb1['x1'] + 1) * (bb1['y2'] - bb1['y1'] + 1)
    bb2_area = (bb2['x2'] - bb2['x1'] + 1) * (bb2['y2'] - bb2['y1'] + 1)

    # compute the intersection over union by taking the intersection
    # area and dividing it by the sum of prediction + ground-truth
    # areas - the interesection area
    iou = intersection_area / float(bb1_area + bb2_area - intersection_area)
    assert iou >= 0.0
    assert iou <= 1.0
    return iou


def get_recall(threshold_iou, test_model, test_dataset, device):
    total_boxes, found_boxes = 0, 0

    for tile_counter in range(len(test_dataset)):
        img, target = test_dataset[tile_counter]
        test_model.eval()
        with torch.no_grad():
            prediction = test_model([img.to(device)])

        predicted_boxes = prediction[0]['boxes'].cpu().detach().numpy()
        gt_boxes = target['boxes'].cpu().detach().numpy()

        for box in gt_boxes:
            total_boxes += 1
            gt_box = {'x1':box[0], 'y1':box[1], 'x2':box[2], 'y2':box[3]}
            if len(predicted_boxes) > 0:
                iou = np.zeros(len(predicted_boxes))
                for i in range(len(predicted_boxes)):
                    pred_box = {'x1':predicted_boxes[i][0], 'y1':predicted_boxes[i][1], \
                                'x2':predicted_boxes[i][2], 'y2':predicted_boxes[i][3]}
                    iou[i] = get_iou(gt_box, pred_box)
                if np.max(iou) >= threshold_iou:
                    found_boxes += 1
            else:
                found_boxes += 0

    return(found_boxes/total_boxes)


def get_prescision(threshold_iou, test_model, test_dataset, device):
    num_preds, num_true_preds = 0, 0
    for tile_counter in range(len(test_dataset)):
        img, target = test_dataset[tile_counter]
        test_model.eval()
        with torch.no_grad():
            prediction = test_model([img.to(device)])

        predicted_boxes = prediction[0]['boxes'].cpu().detach().numpy()
        gt_boxes = target['boxes'].cpu().detach().numpy()

        for box in predicted_boxes:
            num_preds += 1
            pred_box = {'x1':box[0], 'y1':box[1], 'x2':box[2], 'y2':box[3]}
            if len(gt_boxes) > 0:
                iou = np.zeros(len(gt_boxes))
                for i in range(len(gt_boxes)):
                    gt_box = {'x1':gt_boxes[i][0], 'y1':gt_boxes[i][1], \
                                'x2':gt_boxes[i][2], 'y2':gt_boxes[i][3]}
                    iou[i] = get_iou(pred_box, gt_box)
                if np.max(iou) >= threshold_iou:
                    num_true_preds += 1

    return(num_true_preds/num_preds)


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


def read_raster(raster_path):
    """
    Read raster file
    get the meta information and image data
    change the dimensions of the image data from D, H, W to H, W, D and drop the 4th channel
    return image data in numpy format, meta information, shape, and transform
    """
    with rasterio.open(raster_path) as src:
        ras_meta = src.profile
        org_data = src.read()
        img_shape = src.shape
        transform = src.transform
    
    image_data = org_data.copy()    
    image_data = np.moveaxis(image_data, 0, -1)[:, :, 0:3]
    
    return image_data, ras_meta, img_shape, transform


def split_image(image, tile_size):
    height, width = image.shape[0], image.shape[1]
    # use np.ceil, the tile size will between 512-1024
    num_row = np.max([int(np.floor(height/tile_size)), 1])
    num_col = np.max([int(np.floor(width/tile_size)), 1])
    # use np.ceil to make sure the tile_height*num_row is slightly larger than height, and tile_width*num_col is slightly larger than width
    tile_height = int(np.ceil(height/num_row))
    tile_width = int(np.ceil(width/num_col))

    # print(tile_height*num_row, height, tile_width*num_col, width)
    tile_list = []
    for i in range(num_row):
        for j in range(num_col):
            # print(i*tile_height, min((i+1)*tile_height, height),  j*tile_width, min((j+1)*tile_width, width))
            tile = image[i*tile_height:min((i+1)*tile_height, height), j*tile_width:min((j+1)*tile_width, width), :]
            # print(tile.shape)
            tile_list.append(tile)

    # return
    return tile_list, num_row, num_col


def split_image_tensor(image, tile_size):
    height, width = image.shape[1], image.shape[2]
    # use np.ceil, the tile size will between 512-1024
    num_row = np.max([int(np.floor(height/tile_size)), 1])
    num_col = np.max([int(np.floor(width/tile_size)), 1])
    # use np.ceil to make sure the tile_height*num_row is slightly larger than height, and tile_width*num_col is slightly larger than width
    tile_height = int(np.ceil(height/num_row))
    tile_width = int(np.ceil(width/num_col))

    # print(tile_height*num_row, height, tile_width*num_col, width)
    tile_list = []
    for i in range(num_row):
        for j in range(num_col):
            # print(i*tile_height, min((i+1)*tile_height, height),  j*tile_width, min((j+1)*tile_width, width))
            tile = image[:, i*tile_height:min((i+1)*tile_height, height), j*tile_width:min((j+1)*tile_width, width)]
            # print(tile.shape)
            tile_list.append(tile)

    # return
    return tile_list, num_row, num_col


def reconstruct_image(tile_list, num_row, num_col):
    img_rows = []
    for i in range(num_row):
        sub_tile_list = tile_list[i*num_col:(i+1)*num_col]
        # print('row ', str(i), ':')
        # for tile in sub_tile_list:
        #     print(tile.shape)
            
        # print(len(sub_tile_list))
        current_row = np.concatenate(sub_tile_list, axis=1)
        img_rows.append(current_row)

    image = np.concatenate(img_rows, axis=0)

    return image

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


def building_count_png(png_path, model_path, device, tile_size=512, radius=25, mode='all'):
    """
    
    """
    # read raster data according to raster_path
    image_data = np.array(Image.open(png_path))

    # move axis for pytorch [h,w,c]->[c,h,w]
    image_data = np.moveaxis(image_data, -1, 0)

    # normalize the value [0,255]->[0,1]
    image_tensor = torch.tensor(image_data/255).float()

    # split the original image to list of tiles, row-first
    tensor_list, row_number, col_number = split_image_tensor(image_tensor, tile_size)

    # load pre-trained model according to model_path
    saved_model = torch.load(model_path);
    saved_model.eval();
    
    predictions = []
    with torch.no_grad():
        for t in range(0, len(tensor_list), 16):
            batch = [x.to(device) for x in tensor_list[t:t+16]]
            predictions = predictions + saved_model(batch)
        
    mask_list, lbl_list = [], []
    for pred in predictions:
        mask = np.squeeze(torch.sum(pred['masks'], dim=0).mul(255).byte().cpu().numpy())
        lbl_mask = np.zeros_like(mask)

        boxes = pred['boxes'].cpu().numpy()
        lbl = pred['labels'].cpu().numpy()

        for i in range(len(lbl)):
            # get center point
            xmin, ymin, xmax, ymax = boxes[i]
            center_x, center_y = int(np.floor(np.average([xmin, xmax]))), int(np.floor(np.average([ymin, ymax])))
            if mode == 'all':
                lbl_mask[center_y, center_x] = 1
            elif mode == 'trad':
                lbl_mask[center_y, center_x] = 1 if lbl[i] == 1 else None
            elif mode =='modern':
                lbl_mask[center_y, center_x] = 1 if lbl[i] == 2 else None
        
        mask_list.append(mask)
        lbl_list.append(lbl_mask)

    lbl_ful = reconstruct_image(lbl_list, row_number, col_number)

    # create a cycle kernel based on distance
    k_size = int(round(radius/PIXEL))
    weights = np.zeros((k_size*2+1, k_size*2+1))
    for i in range(weights.shape[0]):
        for j in range(weights.shape[1]):
            if ((i-k_size)**2 + (j-k_size)**2)**0.5 <= k_size:
                weights[i, j] = 1

    # calculate the building number with GPU
    lbl_full_tensor = torch.from_numpy(lbl_ful).unsqueeze(0).unsqueeze(0)
    lbl_full_tensor = lbl_full_tensor
    weights_tensor = torch.from_numpy(weights)
    weights_tensor = weights_tensor.view(1, 1, weights_tensor.shape[0], weights_tensor.shape[1]).repeat(1, 1, 1, 1)
    building_counts = torch.nn.functional.conv2d(lbl_full_tensor.float().to(device), weights_tensor.float().to(device), padding='same')
    building_counts = np.squeeze(building_counts.cpu().numpy())
    building_counts = building_counts

    # get buliding density
    building_density = building_counts/(pi*(radius**2))

    return building_density


def mrcnnPred(raster_path, model_path, device, smallest_building = 5, tile_size=512,display_progress=False, debug=False):
    """
    Detailed help TBD
    """
    # read raster data according to raster_path
    image_data, ras_meta, img_shape, transform = read_raster(raster_path)

    # move axis for pytorch [h,w,c]->[c,h,w]
    image_data = np.moveaxis(image_data, -1, 0)
    # normalize the value [0,255]->[0,1]
    image_tensor = torch.tensor(image_data/255).float()

    # split the original image to list of tiles, row-first
    tensor_list, row_number, col_number = split_image_tensor(image_tensor, tile_size)

    # load pre-trained model according to model_path
    saved_model = torch.load(model_path);
    saved_model.to(device)

    box_list, lbl_list = [], []

    # set model to evaluation mode
    saved_model.eval();

    # calculate mask and lbl for each tile
    _temp_counter = 0
    with torch.no_grad():
        for x in tensor_list:
            _temp_counter += 1
            if display_progress:
                print('Processing the {}th tile among {}.'.format(_temp_counter, len(tensor_list)))
            # add a dimension for pytorch
            x = torch.unsqueeze(x, dim=0)
            # use GPU
            x = x.to(device)
            pred = saved_model(x)
            
            # get tile mask (this mask won't be used after this)
            mask = np.squeeze(torch.sum(pred[0]['masks'], dim=0).mul(255).byte().cpu().numpy())
            
            # create a zero mask to be filled
            lbl_mask, box_mask = np.zeros_like(mask), np.zeros_like(mask)

            # get bounding boxes and class label
            boxes, lbls = pred[0]['boxes'].cpu().numpy(), pred[0]['labels'].cpu().numpy()

            for box, lbl in zip(boxes, lbls):
                # get center point
                xmin, ymin, xmax, ymax = box
                # double-check size
                if xmax-xmin > smallest_building and ymax-ymin >=smallest_building:
                # get center point of the box
                    center_x, center_y = int(np.floor(np.average([xmin, xmax]))), int(np.floor(np.average([ymin, ymax])))
                    # write to lbl_mask and box_mask
                    lbl_mask[center_y, center_x] = lbl
                    cv2.rectangle(box_mask, (int(xmin), int(ymin)), (int(xmax), int(ymax)), int(lbl), 5)

            box_list.append(box_mask)
            lbl_list.append(lbl_mask)   

    # reconstruct full size lbl image and mask image
    lbl_ful = reconstruct_image(lbl_list, row_number, col_number)
    box_ful = reconstruct_image(box_list, row_number, col_number)

    if debug:
        return box_list, lbl_list 
    else:
        return box_ful, lbl_ful 


def mrcnnPred_mask(raster_path, model_path, device, smallest_building = 0, fb_threshold = 0.25, tile_size=512,display_progress=False):
    """
    Detailed help TBD
    """
    image_data, ras_meta, img_shape, transform = read_raster(raster_path)

    # move axis for pytorch [h,w,c]->[c,h,w]
    image_data = np.moveaxis(image_data, -1, 0)

    # normalize the value [0,255]->[0,1]
    image_tensor = torch.tensor(image_data/255).float()

    # split the original image to list of tiles, row-first
    tensor_list, row_number, col_number = split_image_tensor(image_tensor, tile_size)

    saved_model = torch.load(model_path);
    saved_model.to(device)

    saved_model.eval();

    original_mask_list, mask_list, lbl_mask_list = [], [], []
    detected_counter, kept_counter = 0, 0
    # calculate mask and lbl for each tile
    _temp_counter = 0
    with torch.no_grad():
        for x in tensor_list:
            _temp_counter += 1
            if display_progress:
                print('Processing the {}th tile among {}.'.format(_temp_counter, len(tensor_list)))
            x = torch.unsqueeze(x, dim=0) # add a dimension for pytorch

            pred = saved_model(x.to(device))
            
            # get tile mask (this mask won't be used after this)
            mask_pred = np.squeeze(torch.sum(pred[0]['masks'], dim=0).mul(255).byte().cpu().numpy())

            original_mask_list.append(mask_pred)

            # get bounding boxes and class label
            boxes, lbls = pred[0]['boxes'].cpu().numpy(), pred[0]['labels'].cpu().numpy()
            
            # create a zero mask to be filled
            mask, lbl_mask = np.zeros_like(mask_pred), np.zeros_like(mask_pred)

            # write the foreground and center location to lbl mask
            for box, lbl in zip(boxes, lbls):
                xmin, ymin, xmax, ymax = box
                xmin, ymin, xmax, ymax = round(xmin), round(ymin), round(xmax), round(ymax)
                detected_counter += 1
                # double-check size
                if xmax-xmin > smallest_building and ymax-ymin > smallest_building:
                    center_x, center_y = int(np.floor(np.average([xmin, xmax]))), int(np.floor(np.average([ymin, ymax])))
                    lbl_mask[center_y, center_x] = lbl # write to lbl_mask
                    kept_counter += 1
                    # start to write to mask
                    for i in range(xmin, xmax):
                        for j in range(ymin, ymax):
                            if mask_pred[j, i] >= fb_threshold*255:
                                mask[j, i] = lbl

            mask_list.append(mask)
            lbl_mask_list.append(lbl_mask)

    # reconstruct full size lbl image and mask image
    lbl_mask_ful = reconstruct_image(lbl_mask_list, row_number, col_number)
    mask_ful = reconstruct_image(mask_list, row_number, col_number)
    original_mask_ful = reconstruct_image(original_mask_list,  row_number, col_number)
    print(detected_counter, kept_counter)
    return lbl_mask_ful, mask_ful, original_mask_ful


def count_buildings(label, radius):
    """
    input:
        label---> HxW matrix, if the element equals to 1, then the current location is a building center, 0 otherwise.
        radius--> We count the building numbers in the circle respect the radius value (unit: pixel).
    
    output:
        building_counts--> for each pixel, the value is the building number in the circle with radius equas to radius. 
    """

    building_counts = np.zeros_like(label).astype(int) # initialize building counts as zero
    buildings_locs = np.nonzero(label) # find buildings
    counter = 0
    for row, col in zip(buildings_locs[1], buildings_locs[0]):
        counter += 1
        if counter % 100 == 0:
            print('Processing the {}th building among {} total'.format(counter, len(buildings_locs[0]))) 
        radiationArea = np.zeros_like(label)
        cv2.circle(radiationArea, (row, col), radius, 1, -1)
        building_counts = np.add(building_counts, radiationArea)
    
    return building_counts



def createDensityMapCV(label_mask, radius=25):
    """
    Detailed help TBD
    """
    label_modern, label_trad = deepcopy(label_mask), deepcopy(label_mask)
    radius = round(radius/PIXEL)

    label_modern[label_modern==2] = 0 # traditional buildings (2) will be ignored
    label_trad[label_trad==1] = 0 # modern buildings (1) will be ignored
    # label_all[label_all==2] = 1 # set as 2->1, all buildings are the same
    
    print('Counting buildings. This will take a little while.')
    bldg_count_modern = count_buildings(label_modern, radius)
    bldg_count_trad = count_buildings(label_trad, radius)
    bldg_count_all = np.add(bldg_count_modern, bldg_count_trad)
        
    return bldg_count_modern, bldg_count_trad, bldg_count_all


def createDensityMap(lbl_full, device, radius=25, mode='all'):
    """
    Detailed help TBD
    """
    label = deepcopy(lbl_full)
    if mode == 'all':
        # all traditional building are set as 2->1
        label[label==2] = 1
    elif mode == 'trad':
        label[label==1] = 0
    elif mode == 'modern':
        label[label==2] = 0
        
    # create a cycle kernel based on distance
    k_size = int(round(radius/PIXEL))
    weights = np.zeros((k_size*2+1, k_size*2+1))
    for i in range(weights.shape[0]):
        for j in range(weights.shape[1]):
            if ((i-k_size)**2 + (j-k_size)**2)**0.5 <= k_size:
                weights[i, j] = 1

    # calculate the building number with GPU
    lbl_full_tensor = torch.from_numpy(label).unsqueeze(0).unsqueeze(0)
    # lbl_full_tensor = lbl_full_tensor
    weights_tensor = torch.from_numpy(weights)
    weights_tensor = weights_tensor.view(1, 1, weights_tensor.shape[0], weights_tensor.shape[1]).repeat(1, 1, 1, 1)
    building_counts = torch.nn.functional.conv2d(lbl_full_tensor.float().to(device), weights_tensor.float().to(device), padding='same')
    building_counts = np.squeeze(building_counts.cpu().numpy())

    # get buliding density
    # building_density = building_counts/(pi*(radius**2))
    return building_counts


def building_count_raster_slow(raster_path, model_path, device, tile_size=512, radius=25, mode='all'):
    """
    
    """
    # read raster data according to raster_path
    image_data, ras_meta, img_shape, transform = read_raster(raster_path)

    # move axis for pytorch [h,w,c]->[c,h,w]
    image_data = np.moveaxis(image_data, -1, 0)
    # normalize the value [0,255]->[0,1]
    image_tensor = torch.tensor(image_data/255).float()

    # split the original image to list of tiles, row-first
    tensor_list, row_number, col_number = split_image_tensor(image_tensor, tile_size)

    # load pre-trained model according to model_path
    saved_model = torch.load(model_path);

    mask_list, lbl_list = [], []
    saved_model.eval();
    with torch.no_grad():
        for x in tensor_list:
            # add a dimension for pytorch
            x = torch.unsqueeze(x, dim=0)
            x = x.to(device)
            pred = saved_model(x)
            mask = np.squeeze(torch.sum(pred[0]['masks'], dim=0).mul(255).byte().cpu().numpy())
            lbl_mask = np.zeros_like(mask)

            boxes = pred[0]['boxes'].cpu().numpy()
            lbl = pred[0]['labels'].cpu().numpy()

            for i in range(len(lbl)):
                # get center point
                xmin, ymin, xmax, ymax = boxes[i]
                center_x, center_y = int(np.floor(np.average([xmin, xmax]))), int(np.floor(np.average([ymin, ymax])))
                if mode == 'all':
                    lbl_mask[center_y, center_x] = 1
                elif mode == 'trad':
                    lbl_mask[center_y, center_x] = 1 if lbl[i] == 1 else None
                elif mode =='modern':
                    lbl_mask[center_y, center_x] = 1 if lbl[i] == 2 else None
            
            mask_list.append(mask)
            lbl_list.append(lbl_mask)   

    lbl_ful = reconstruct_image(lbl_list, row_number, col_number)

    # create a cycle kernel based on distance
    k_size = int(round(radius/PIXEL))
    weights = np.zeros((k_size*2+1, k_size*2+1))
    for i in range(weights.shape[0]):
        for j in range(weights.shape[1]):
            if ((i-k_size)**2 + (j-k_size)**2)**0.5 <= k_size:
                weights[i, j] = 1

    # calculate the building number with GPU
    lbl_full_tensor = torch.from_numpy(lbl_ful).unsqueeze(0).unsqueeze(0)
    lbl_full_tensor = lbl_full_tensor
    weights_tensor = torch.from_numpy(weights)
    weights_tensor = weights_tensor.view(1, 1, weights_tensor.shape[0], weights_tensor.shape[1]).repeat(1, 1, 1, 1)
    building_counts = torch.nn.functional.conv2d(lbl_full_tensor.float().to(device), weights_tensor.float().to(device), padding='same')
    building_counts = np.squeeze(building_counts.cpu().numpy())

    # get buliding density
    # building_density = building_counts/(pi*(radius**2))
    return building_counts


def building_count_raster(raster_path, model_path, device, tile_size=512, radius=25, mode='all'):
    """
    
    """
    # read raster data according to raster_path
    image_data, ras_meta, img_shape, transform = read_raster(raster_path)

    # move axis for pytorch [h,w,c]->[c,h,w]
    image_data = np.moveaxis(image_data, -1, 0)
    # normalize the value [0,255]->[0,1]
    image_tensor = torch.tensor(image_data/255).float()

    # split the original image to list of tiles, row-first
    tensor_list, row_number, col_number = split_image_tensor(image_tensor, tile_size)

    # load pre-trained model according to model_path
    saved_model = torch.load(model_path);
    saved_model.eval();
    with torch.no_grad():
        predictions = saved_model([x.to(device) for x in tensor_list])
    
    mask_list, lbl_list = [], []
    for pred in predictions:
        mask = np.squeeze(torch.sum(pred['masks'], dim=0).mul(255).byte().cpu().numpy())
        lbl_mask = np.zeros_like(mask)

        boxes = pred['boxes'].cpu().numpy()
        lbl = pred['labels'].cpu().numpy()

        for i in range(len(lbl)):
            # get center point
            xmin, ymin, xmax, ymax = boxes[i]
            center_x, center_y = int(np.floor(np.average([xmin, xmax]))), int(np.floor(np.average([ymin, ymax])))
            if mode == 'all':
                lbl_mask[center_y, center_x] = 1
            elif mode == 'trad':
                lbl_mask[center_y, center_x] = 1 if lbl[i] == 1 else None
            elif mode =='modern':
                lbl_mask[center_y, center_x] = 1 if lbl[i] == 2 else None
        
        mask_list.append(mask)
        lbl_list.append(lbl_mask)

    lbl_ful = reconstruct_image(lbl_list, row_number, col_number)

    # create a cycle kernel based on distance
    k_size = int(round(radius/PIXEL))
    weights = np.zeros((k_size*2+1, k_size*2+1))
    for i in range(weights.shape[0]):
        for j in range(weights.shape[1]):
            if ((i-k_size)**2 + (j-k_size)**2)**0.5 <= k_size:
                weights[i, j] = 1

    # calculate the building number with GPU
    lbl_full_tensor = torch.from_numpy(lbl_ful).unsqueeze(0).unsqueeze(0)
    lbl_full_tensor = lbl_full_tensor
    weights_tensor = torch.from_numpy(weights)
    weights_tensor = weights_tensor.view(1, 1, weights_tensor.shape[0], weights_tensor.shape[1]).repeat(1, 1, 1, 1)
    building_counts = torch.nn.functional.conv2d(lbl_full_tensor.float().to(device), weights_tensor.float().to(device), padding='same')
    building_counts = np.squeeze(building_counts.cpu().numpy())
    building_counts = building_counts

    # get buliding density
    building_density = building_counts/(pi*(radius**2))
    return building_density


def save_density_map(original_raster_path, density_map):
    # normalize density map
    to_be_saved = (density_map/np.max(density_map)*255).astype(int)
    # read the original raster and get meta
    original_raster = rasterio.open(original_raster_path)
    original_meta_info = original_raster.meta
    new_meta_info = original_meta_info.copy()
    # close the original raster
    original_raster.close()
    # single band
    new_meta_info['count'] = 1
    new_meta_info['dtype'] = 'float32'
    # create the name
    density_map_path = original_raster_path.replace('.tif', '_density_map.tif')
    # write density_map
    with rasterio.open(density_map_path, 'w', **new_meta_info) as dst:
        dst.write(to_be_saved, 1)
    dst.close


def save_density_map_batch(original_raster_path, density_map, detection_boxes, target_folder, raster_name, radius, model_name=None):
    # read the original raster and get meta
    original_raster = rasterio.open(original_raster_path)
    original_meta_info = original_raster.meta
    new_meta_info = original_meta_info.copy()
    # close the original raster
    original_raster.close()
    # set single band
    new_meta_info['count'] = 1
    new_meta_info['dtype'] = 'float32'

    # save density map
    if model_name != None:
        surf = raster_name + '_' + model_name +  + '_' + str(radius) + '_building_counts.tif'
    else:
        surf = raster_name + '_' + str(radius) + '_building_counts.tif'

    density_map_path = path.join(target_folder, surf)
    # write density_map
    with rasterio.open(density_map_path, 'w', **new_meta_info) as dst:
        dst.write(density_map.astype(float), 1)
    dst.close

    # save boxes
    if model_name != None:
        surf = raster_name + '_' + model_name + '_boxes.tif'
    else:
        surf = raster_name + '_' + str(radius) + '_boxes.tif'
        
    detection_boxes_path = path.join(target_folder, surf)
    # write density_map
    with rasterio.open(detection_boxes_path, 'w', **new_meta_info) as dst:
        dst.write(detection_boxes.astype(float), 1)
    dst.close


def save_density_map_3bands(original_raster_path, density_maps, detection_boxes, target_folder, raster_name, radius, model_name=None):
    # read the original raster and get meta
    original_raster = rasterio.open(original_raster_path)
    original_meta_info = original_raster.meta
    new_meta_info = original_meta_info.copy()
    # close the original raster
    original_raster.close()
    # set single band
    new_meta_info['count'] = 3
    new_meta_info['dtype'] = 'float32'

    # to_be_saved = np.moveaxis(np.array(density_maps), 0, -1)

    # save density map
    if model_name != None:
        surf = raster_name + '_' + model_name +  + '_' + str(radius) + '_building_counts.tif'
    else:
        surf = raster_name + '_' + str(radius) + '_building_counts.tif'

    density_map_path = path.join(target_folder, surf)
    # write density_map
    with rasterio.open(density_map_path, 'w', **new_meta_info) as dst:
        dst.write(density_maps[0].astype(float), 1)
        dst.write(density_maps[1].astype(float), 2)
        dst.write(density_maps[2].astype(float), 3)
    dst.close

    # save boxes
    if model_name != None:
        surf = raster_name + '_' + model_name + '_boxes.tif'
    else:
        surf = raster_name + '_' + str(radius) + '_boxes.tif'
        
    detection_boxes_path = path.join(target_folder, surf)
    # write density_map
    with rasterio.open(detection_boxes_path, 'w', **new_meta_info) as dst:
        dst.write(detection_boxes.astype(float), 1)
    dst.close


def save_building_counts_downsample(original_raster_path, building_counts, target_folder, raster_name, radius, model_name=None):
    """
    
    """
    # create raster name
    if model_name != None:
        building_count_name = raster_name + '_' + model_name +  + '_' + str(radius) + '_building_counts.tif'
    else:
        building_count_name = raster_name + '_' + str(radius) + '_building_counts.tif'

    building_count_path = path.join(target_folder, building_count_name)

    res = 1 if radius <=200 else 10 #get resolution based on radius

    factor = PIXEL/res # get transform factor

    # get original raster meta
    with rasterio.open(original_raster_path) as src_ds:
        # data = src_ds.read()
        profile = src_ds.profile
        transform = src_ds.transform
        height, width = src_ds.height, src_ds.width
        crs = src_ds.crs

    # create new raster_meta
    new_height, new_width = int(height * factor), int(width * factor)
    new_transform = transform * transform.scale(
                        (width / new_width),
                        (height / new_height)
                    )

    new_profile = copy(profile)
    new_profile.update(transform=new_transform, driver='GTiff', height=new_height, width=new_width, crs=crs, count=3, dtype='float32')
    src_ds.close()
    
    # resize the building counts
    building_count_modern = cv2.resize(building_counts[0].astype('float'), (new_width, new_height))
    building_count_trad = cv2.resize(building_counts[1].astype('float'), (new_width, new_height))
    building_count_all = np.add(building_count_modern, building_count_trad)

    with rasterio.open(building_count_path, 'w', **new_profile) as dst_ds:
        dst_ds.write(building_count_modern, 1)
        dst_ds.write(building_count_trad, 2)
        dst_ds.write(building_count_all, 3)
    dst_ds.close()


def save_building_counts(original_raster_path, building_counts, target_folder, raster_name, radius, model_name=None):
    """
    
    """
    # read the original raster and get meta
    original_raster = rasterio.open(original_raster_path)
    original_meta_info = original_raster.meta
    new_meta_info = original_meta_info.copy()
    original_raster.close()  # close the original raster

    # modify the meta information
    new_meta_info['count'] = 3
    new_meta_info['dtype'] = 'float32'

    # create raster name
    if model_name != None:
        building_count_name = raster_name + '_' + model_name +  + '_' + str(radius) + '_building_counts.tif'
    else:
        building_count_name = raster_name + '_' + str(radius) + '_building_counts.tif'

    building_count_path = path.join(target_folder, building_count_name)

    with rasterio.open(building_count_path, 'w', **new_meta_info) as dst:
        dst.write(building_counts[0].astype(float), 1)
        dst.write(building_counts[1].astype(float), 2)
        dst.write(building_counts[2].astype(float), 3)
    dst.close


def save_shape_file(original_raster_path, mask, target_folder, raster_name):
    """
    mask is the predicted label that includes values 0-2
    """
    src = rasterio.open(original_raster_path)
    
    write_type = 'w'
    site = raster_name
    schema = {
        'geometry': 'Polygon',
        'properties': {'id': 'int', 'Type':'str', 'Site':'str'},
    }

    
    # modern mask
    modern_mask = deepcopy(mask)
    modern_mask[modern_mask==2] = 0

    # traditional mask
    trad_mask = deepcopy(mask)
    trad_mask[trad_mask==1] = 0

    shape_id = 0
    shape_file_name = raster_name + '_'  + 'buildings.shp'
    with fiona.open(path.join(target_folder, shape_file_name), write_type, 'ESRI Shapefile', schema) as shp:
        # process modern buildings
        contours, _ = cv2.findContours(modern_mask, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)
        for contour in contours:
            if cv2.contourArea(contour) >= 100: # modern building size threshold 535 ~ 5x5 m^2
                build_type = 'modern_build'
                rect = cv2.minAreaRect(contour)
                box = np.int0(cv2.boxPoints(rect))
                x, y = box[:, 0], box[:, 1]
                # write current shape
                lon, lat = rasterio.transform.xy(src.transform, y,x)
                coords = [(lon[i], lat[i]) for i in range(len(lon))]
                poly = Polygon(coords)
                shp.write({
                            'geometry': mapping(poly),
                            'properties': {'id': shape_id, 'Type':build_type, 'Site':site}
                        })
                shape_id += 1
            else:
                continue
            
        # process modern buildings
        contours, _ = cv2.findContours(trad_mask, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)
        for contour in contours:
            if cv2.contourArea(contour) >= 30: # traditional building size threshold 325 ~ 3x3 m^2
                build_type = 'trad_build'
                (Cx, Cy),radius = cv2.minEnclosingCircle(contour)
                center = (int(Cx),int(Cy))
                radius = int(radius)
                x = [Cx + radius * cos(radians(ang)) for ang in range(0, 360, 60)]
                y = [Cy + radius * sin(radians(ang)) for ang in range(0, 360, 60)]
                # write current shape
                lon, lat = rasterio.transform.xy(src.transform, y,x)
                coords = [(lon[i], lat[i]) for i in range(len(lon))]
                poly = Polygon(coords)
                shp.write({
                            'geometry': mapping(poly),
                            'properties': {'id': shape_id, 'Type':build_type, 'Site':site}
                        })
                shape_id += 1
            else:
                continue

    shp.close()
    src.close()
    return shape_id


def save_mask(original_raster_path, mask, target_folder, raster_name):
    """
    
    """
    # read the original raster and get meta
    original_raster = rasterio.open(original_raster_path)
    original_meta_info = original_raster.meta
    new_meta_info = original_meta_info.copy()
    original_raster.close()  # close the original raster

    # modify the meta information
    new_meta_info['count'] = 1
    new_meta_info['dtype'] = 'float32'

    mask_file_name = raster_name + '_mask.tif'

    mask_file_path = path.join(target_folder, mask_file_name)

    with rasterio.open(mask_file_path, 'w', **new_meta_info) as dst:
        dst.write(mask.astype(float), 1)
        
    dst.close