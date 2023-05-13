import numpy as np
import torch, torchvision
from mask_rcnn_utils import *
import matplotlib.pyplot as plt
import rasterio
from glob import glob
from os.path import join
import os
from random import choice
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("--tile_size", type=int, default=512)
parser.add_argument("--radius", type=int, default=25)
parser.add_argument("--device", type=str, default='cuda')
parser.add_argument("--raster_path", type=str, required=True)
parser.add_argument("--model_path", type=str, required=True)
parser.add_argument("--output_dir", typer=str, required=True)

def main():
    args = parser.parse_args()

    if args.device == 'cuda' and torch.torch.cuda.is_available():
        device = torch.device('cuda')
    else:
        device = 'cpu'    
    boxes, label = mrcnnPred(args.raster_path, args.model_path, device)

    density_map_modern = createDensityMap(label, device, mode='modern')
    density_map_trad = createDensityMap(label, device, mode='trad')
    density_map_all = createDensityMap(label, device, mode='all')

    save_density_map_3bands(args.raster_path, [density_map_modern, density_map_trad, density_map_all], boxes, args.output_dir, args.raster_name, args.radius)


if __name__ == "__main__":
    main()
            