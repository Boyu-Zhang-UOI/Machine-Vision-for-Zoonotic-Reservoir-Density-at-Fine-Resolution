import numpy as np
import torch, torchvision
from mask_rcnn_utils import *
import matplotlib.pyplot as plt
import rasterio
from glob import glob
from os.path import join
import os
from random import choice

# global variables
tile_size = 512
ridus = 25
device = torch.device('cuda') if torch.cuda.is_available() else torch.device('cpu')


# use new Bing raster location
bing_folder = '/mnt/ceph/onehealth/MV_PREEMPT/Landcover_Classifier/Data/Bing_Raster_Images/'
# no google rasters needed in this notebook
# google_folder = '/mnt/ceph/boyuz/Builiding_Landscaping/building_dataset/Google_Raster_Images'
# use the trained models
model_folder = '/mnt/ceph/boyuz/Builiding_Landscaping/trained_models'
# set the target folder to contain the results
result_folder = '/mnt/ceph/onehealth/Boyu/mrcnn/mask_density_map/'


# list rasters in new location
raster_list = list(glob(join(bing_folder, '*.tif')))
raster_list = ['/mnt/ceph/onehealth/MV_PREEMPT/Landcover_Classifier/Data/Bing_Raster_Images/Bafodia.tif']
# list all models
model_list = list(glob(join(model_folder, '30_epoch_2048_*')))
print('There are {} models and {} rasters.'.format(len(model_list), len(raster_list)))

radii_list = [25, 50, 100, 200]

# for r in [25]:
for r in [25, 50, 100, 200]:
    for raster_path in raster_list:
        model_path = choice(model_list)
        raster_name = raster_path.split(os.sep)[-1][:-4]
        model_name = model_path.split(os.sep)[-1][14:]
        print(raster_name, model_name)
        boxes, label = mrcnnPred(raster_path, model_path, device)

        density_map_modern = createDensityMap(label, device, mode='modern')
        density_map_trad = createDensityMap(label, device, mode='trad')
        density_map_all = createDensityMap(label, device, mode='all')

        save_density_map_3bands(raster_path, [density_map_modern, density_map_trad, density_map_all], boxes, result_folder, raster_name, r)
