## Make images that can be used in an overview slide
library(fields)
library(raster)
library(RColorBrewer)
library(viridis)
library(rgdal)
library(ggplot2)

#set color scheme for plots
seg.breaks=0.5 + c(-1:8) 
col.vect <- c('yellow','hotpink1', 'purple', 'darkgray', 'red', 'white', 'skyblue', 'orange' )
cols = colorRampPalette(col.vect)
minv <- 0 ## min value plotted
pred.nbreaks = 10 ## Number of color breaks in prediction plot
height = 3 ## Image height

plot.path = 'Figures_agg'
suffix = ''


source("Tools/Functions.r")

## Get trap data
All.dat <- read.csv('../../Trap_Data/Data/Clean_Trap_Data/Clean_Both_Data_By_Trap.csv')
All.dat <- subset(All.dat, Site %in% c('Bantou', 'Tanganya', 'Bafodia'))

for(site.name in c('Bantou', 'Tanganya', 'Bafodia')){
    
    ## Load TIF image of site    
    site.rast <- brick(paste0('../Data/Rasters/Satellite_Images/Large_Bing_Images/',
                              site.name,'.tif'))
    
    ## Get extent information for prediction
    pred_out <- raster::raster(paste0('../Predictions/',
                                      site.name, '.tif'))
    crop.ext = extent(pred_out)

    maxv <- max(as.vector(pred_out), na.rm = TRUE)
    pred.breaks = seq(minv, maxv, length.out = pred.nbreaks)
    
    xlim = c(crop.ext@xmin,crop.ext@xmax)
    ylim = c(crop.ext@ymin,crop.ext@ymax)
    x2y <- diff(xlim)/diff(ylim)

    
    ## First a simple image of the location
    png(file = paste0(plot.path, '/', site.name,suffix,'.png'), width = x2y*height, height = height,
        units = 'in', res = 300)
    raster::plotRGB(site.rast, ext = crop.ext)
    dev.off()
    
    ## Add in segmentation
    buildings.shp <- readOGR(dsn = '../Data/Rasters/Satellite_Features/Building',
                             layer = paste0(site.name, '_buildings'))
    
    site.buildings <- buildings.shp[buildings.shp$Site==site.name,]
    col.buildings <- ifelse(site.buildings$Type=='modern_build', 'red', 'blue')
    
    rast.loc <- paste0('../Data/Rasters/Satellite_Features/Land_cover_features/',
                       tolower(site.name), '_lc_fea_25m_res_1m.tif')
    lc.site <- raster::stack(rast.loc)
    lc.crop <- crop(lc.site, crop.ext)
    lc.crop <- lc.crop[[9]]
    
    
    ## Tanganya image with segmentation overlaid
    png(file = paste0(plot.path, '/', site.name,suffix,'_seg.png'), width = x2y*height,
        height = height,
        units = 'in', res = 300)
    par(mai = c(0,0,0,0))
    raster::plotRGB(crop(site.rast, crop.ext), ext = crop.ext, axes = FALSE)
    plot(crop(lc.crop, crop.ext), add = TRUE, alpha = 0.5, legend = FALSE, breaks = seg.breaks,
         col = cols(8))
    plot(site.buildings, add = TRUE, border = col.buildings)
    dev.off()
    
    
    ## Heatmap predictions
    png(file = paste0(plot.path, '/', site.name,suffix,'_pred.png'), width = x2y*height,
        height = height,
        units = 'in', res = 300)
    par(mai = c(0.0,0,0.0,0))
    raster::plotRGB(crop(site.rast, crop.ext), ext = crop.ext, axes = FALSE, margins = TRUE)
    plot(crop(pred_out, crop.ext), add = TRUE, alpha = 0.75, legend = FALSE, breaks = pred.breaks,
         col = viridis(pred.nbreaks - 1))
    dev.off()
    
    png(file = paste0(plot.path, '/', site.name,suffix,'_leg.png'), width = 1,
        height = 3,
        units = 'in', res = 300)
    par(mai = c(0.0,0,0.0,0))
    fake.mat = matrix(pred.breaks, nrow = 1)
    image.plot(fake.mat, xaxt = 'n', col = viridis(pred.nbreaks - 1),
               legend.only = TRUE, add = TRUE, legend.lab = 'Mn trap success',
               legend.line = 2.8,
               main = '')
    dev.off()
    print(site.name)
} ## End loop through towns


## Make a legend
png(file = paste0(plot.path, '/', 'Seg_legend.png'), width = 1.5, height = 4, units = 'in',
    res = 400)
par(mai = c(0,0,0,0))
base::plot(NA, xlim = c(0,1), ylim = c(0,1), axes = FALSE,
           xlab = '', ylab = '')
legend(legend = c('Modern', 'Traditional'), x =0.1, y = 0.9,
       pch = c(22, 21), pt.cex = 2, pt.bg = 'white', col = c('red', 'blue'), title = 'Buildings'
       )
legend(legend = c('Bare', 'Grass', 'Tree', 'Burned', 'Rice',
                                'Cloud', 'Water', 'Cultivation'),
       x =0.1, y = 0.6,
       pch = 22, col = 'black', pt.bg = col.vect, pt.cex = 2, title = 'Land Cover'
       )
dev.off()

## -- Make convolution example figures

for(radius in c(25,50,100,200,500,1000)){
    supp = 'm_res_1m'
    if(radius > 100){supp = 'm_res_10m'}
    
    
    rast.loc <- paste0('../../SkyLens_v4/Boyu_Predictions/Land_cover_features/',
                       tolower(site.name), '_lc_fea_', radius, supp, '.tif')
    lc.site <- raster::stack(rast.loc)
    lc.crop <- crop(lc.site, crop.ext)
    lc.crop <- lc.crop[[5]] ## 5 = rice
    vmax = 1#maxValue(lc.crop)
    conv.breaks = seq(0,vmax,length = 10)

    png(file = paste0(plot.path, '/', site.name,suffix,'_conv_', radius,'.png'),
        width = 5, height = 5,
        units = 'in', res = 400)
    par(mai = c(0.5,0,0.5,1))
    raster::plotRGB(crop(site.rast, crop.ext), ext = crop.ext, axes = FALSE, margins = TRUE)
    plot(crop(lc.crop, crop.ext), add = TRUE, alpha = 0.5, legend = FALSE,
         breaks = conv.breaks,
         col = viridis(10))
    fake.mat = matrix(conv.breaks, nrow = 1)
    image.plot(fake.mat, xaxt = 'n', col = viridis(nbreaks - 1),
               legend.only = TRUE, add = TRUE,
               legend.line = 2.8,
               main = '')
    dev.off()
    print(radius)
}## End loop through radii





## ## --- Google aerial image of Bafodia

## site.name = unique(train.data[wi.eval.traps,'Site'])
## print(paste0('Focal trap site: ', site.name))
## site.rast <- brick(paste0('../SkyLens_v3/Data/Rasters/Google_Raster_Images/',
##                           site.name,'.tif'))
## dir.create(plot.path, recursive = TRUE, showWarnings = FALSE)
## png(file = paste0(plot.path, '/test_validation', fold.i, '.png'), width = 6, height = 6,
##     units = 'in', res = 300)
## par(mai = c(0,0,0,0))
## plotRGB(site.rast)
## points(train.data[fold.matrix[,fold.i]==fold.i, c('Longitude','Latitude')],
##        pch = 1, cex = 0.5, 
##        col = 'orange')
## points(train.data[fold.matrix[,fold.i]==(fold.i + 1), c('Longitude','Latitude')],
##        pch = 1, cex = 0.5, 
##        col = 'black')
## points(train.data[is.na(fold.matrix[,fold.i]), c('Longitude','Latitude')],
##        pch = 1, cex = 0.5,
##        col = 'green')
## points(train.data[focal.trap,c('Longitude','Latitude')],
##        pch = 1, cex = 0.5, 
##        col = ifelse(train.data[focal.trap,'Mna']==1, 'red', 'blue'))
## dev.off()


