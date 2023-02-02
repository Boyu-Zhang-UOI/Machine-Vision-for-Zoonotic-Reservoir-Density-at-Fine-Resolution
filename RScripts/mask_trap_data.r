## This script loads the cleaned trap data and crops it to the sites
## and columns needed to fit the XGB models. 

## This script can be removed.

dat = read.csv(file = '../../Trap_Data/Data/Clean_Trap_Data/Clean_Both_Data_By_Trap.csv')
data.table::setDT(dat)

## Get useful functions
source("Tools/Functions.r")

## Mask the full trapping data to Bantou, Tanganya, and Bafodia with ind_coords = TRUE

dat.mask = dat[Site %in% c('Bantou', 'Bafodia', 'Tanganya') & ind_coords==TRUE,]

## Only keep predictors that we need
## - satellite, rainfall
lc.names = c('Frac_bare', 'Frac_grass', 'Frac_tree', 'Frac_burn',
             'Frac_rice', 'Frac_water', 'Frac_mound')
bu.names = c('Density_Moderns', 'Density_Traditionals', 'Density_Buildings')
sc.bu.names = paste('sc_', bu.names, sep = '')
precip.lags = paste('P', 1:12, sep = '')

lc.features = get.features(rvec = c(25,50,100,200,500,1000), names = lc.names)
build.features = get.features(rvec = c(25,50,100,200,500,1000), names = bu.names)
sc.build.features = get.features(rvec = c(25,50,100,200,500,1000), names = sc.bu.names)

col.names = c('Source', 'Site', 'Country', 'Longitude', 'Latitude',
                       'ind_coords', 'Type', 'Track_ID', 'Trap_ID', 'Visit',
                       'Date', 'Night', 'Rodent', 'Sp', 'Mna', 'Trap.weight',
                       'Species_ID', 'Genus', 'Species', 'House',
              lc.features, build.features, sc.build.features,
              precip.lags)
dat.crop = dat.mask[,..col.names]

write.csv(dat.crop, file = '../Data/Trap_Data/Clean_Both_Data_By_Trap.csv')
