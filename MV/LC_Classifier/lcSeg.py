import os
import pandas as pd
import numpy as np

from PIL import Image
import imageio.v2 as imageio

from copy import deepcopy
import torch
import torch.nn.functional as F

from torchvision.models.segmentation.deeplabv3 import DeepLabHead, FCNHead
from torchvision import models
from torchvision.transforms import ToTensor, InterpolationMode

from torch.utils.data import Dataset

# define dataset
class segDataset(Dataset):

    def __init__(self, dataframe, transforms=None):
        self.image_list = list(dataframe['Image'])
        self.mask_list = list(dataframe['Label'])
        self.transforms = transforms

    def __len__(self):
        return len(self.image_list)

    def __getitem__(self, idx):
        image_path = self.image_list[idx]
        mask_path = self.mask_list[idx]

        image = Image.open(image_path).convert("RGB")
        mask = np.array(Image.open(mask_path).convert("L"), dtype=np.int32)

        if self.transforms is not None:
            image = self.transforms(image)
            # mask = np.squeeze(self.transforms(mask))

        return (image, mask)


# define dataset
class segDatasetWeight(Dataset):

    def __init__(self, dataframe, tTransforms=None, aTransforms=None):
        self.image_list = list(dataframe['Image'])
        self.mask_list = list(dataframe['Label'])
        self.weight_list = list(dataframe['Weight'])
        self.tTransforms = tTransforms
        self.aTransforms = aTransforms

    def __len__(self):
        return len(self.image_list)

    def __getitem__(self, idx):
        image_path = self.image_list[idx]
        mask_path = self.mask_list[idx]
        weight_path = self.weight_list[idx]

        image = imageio.imread(image_path, pilmode = 'RGB')
        mask = np.squeeze(np.asarray(imageio.imread(mask_path, pilmode = 'RGB')[:,:,0]))
        weight = np.squeeze(np.asarray(imageio.imread(weight_path, pilmode = 'RGB')[:,:,0]))
        
        if self.aTransforms is not None:
            transformed = self.aTransforms(image=image, masks=[mask, weight])
            image = transformed['image']
            mask, weight = transformed['masks']

        if self.tTransforms is not None:
            image = self.tTransforms(image)
            # mask = self.transforms(mask)
            # weight = self.transforms(weight)

        return (image, (mask, weight))


def slowOneHot(input_mask, num_classes):
    '''
    This function return a num_classes + 1 x H x W matrix as one-hot label for semantic segmentation
    '''
    # input check
    if input_mask.ndim != 2:
        raise Exception('input dimention incorrect')

    one_hot_mask = np.zeros((num_classes+1, input_mask.shape[0], input_mask.shape[1]))

    # put the class number of no-label to the end of list
    input_mask = deepcopy(input_mask)
    input_mask[input_mask==255] = num_classes
    
    # fill output
    for i in range(num_classes+1):
        one_hot_mask[i, input_mask==i] = 1

    return one_hot_mask


def _to_one_hot(y, num_classes):
    y = torch.squeeze(y)
    one_hot_tensor = F.one_hot(y.to(torch.int64), num_classes=9)
    return one_hot_tensor.permute(2, 0, 1)

def Crop_Image(img, x, y, width, height = None):
    """
    Crop image
    
    Parameters: 
        -img: image array (dim H x W x C)
        -x,y: upper left corner of crop rectangle
        -width, height: width and height of cropping rectangle
    
    Returns:
        -Cropped image of dimension H x W x C
    """
    
    if height == None:
        height = width
    
    img = img[y:y+height, x:x+width,:]
    return img


def resample_dataframe(df, feature_dict, thresh_list, desired_props = None, by_pixels = False, nsamps = 1, nrows = None):
    """
    Crop image and save PNG
    
    Parameters: 
        -df: dataset
        -feature_dict:
        -thresh_list: list of threshold values for an image to be considered to contain a feature
        -desired_props: desired proportions of each feature type
        -by_pixels: Whether to use pixel counts rather than feature-presence feature-absence criterion for inclusion
        -nsamps: initial number of samples 
        -nrows: number of entries in final returned dataset
        
    Return: 
        -resampled dataframe
    """
    
    resample_df = df.iloc[0:nsamps].copy()
    cur_size = resample_df.shape[0]

    if desired_props is None:
        desired_props = 1/len(feature_dict)*np.ones(len(feature_dict))
    else:
        desired_props = desired_props / np.sum(desired_props)
        print(desired_props)   
        
    if nrows is None:
        nrows = df.shape[0]
        
    while cur_size < nrows: 
        if by_pixels==True:
            current_nums = (resample_df[feature_dict.values()]).sum(axis = 0)
        else:
            current_nums = (resample_df[feature_dict.values()] > thresh_list).sum(axis = 0)    
        current_nums = current_nums/current_nums.sum()
        
        current_discrepancy = desired_props - current_nums
        get_group = current_discrepancy.argmax()
        get_mask = df[feature_dict[str(get_group)]] > thresh_list[get_group]
        get_df = df.loc[get_mask,:]
        new_samps = get_df.sample(n = nsamps, replace = True)
        resample_df = pd.concat((resample_df, new_samps), axis = 0)
        cur_size = resample_df.shape[0]
    
    #print(current_nums)    
    return resample_df.copy()