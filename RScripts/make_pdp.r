library(ggplot2)
library(stringr)
library(ggh4x)
source("Tools/Functions.r")
dat = read.csv('../Figures_agg/pdp_info.csv')

dat$radius = get.radii(dat$feature)
dat$base_name = get.radii(dat$feature, ind = 1)
dat$name = gsub('Frac_', '', dat$base_name)
dat$name = gsub('Density_', '', dat$base_name)
dat$name = str_to_sentence(dat$name)

data.table::setDT(dat)

dat$type = dat$feature
dat$type[grepl('Frac', dat$feature)] = 'Landcover'
dat$type[grepl('Density', dat$feature)] = 'Buildings'
dat$type[dat$type %in% c('P1','P2','P3','P4','P5','P6','P7','P8','P9','P10','P11','P12')] = 'Rainfall'

lab = function(x){
    paste('Radius: ', x, 'm', sep = '')
    }

## Get average TS for each feature x train_site x val_site x test_site
mean_dat = dat[,.(mts = mean(average)),
               by = list(feature , train_site , val_site , test_site)]

dat <- merge(dat, mean_dat, all.x = TRUE, all.y = FALSE)

dat <- dat[(radius %in% c(25,50,100,200,500,1000)) | (dat$feature %in% c('Night', 'House')),]

dat$name = factor(dat$name,
                  levels = c('Frac_bare', 'Frac_tree', 'Frac_water','Frac_grass',
                             'Frac_rice', 'Frac_mound',
                             'Frac_burn',
                             'Buildings', 'Sc_buildings', 'Moderns', 'Sc_moderns',
                             'Traditionals', 'Sc_traditionals', paste('P', 1:12, sep = ''),
                             'House', 'Night'),
                  ordered = TRUE)

dat$lc1 = dat$name %in% c('Frac_bare', 'Frac_tree', 'Frac_water','Frac_grass')
dat$lc2 = dat$name %in% c('Frac_rice', 'Frac_mound','Frac_burn')

dat$bu1 = dat$name %in% c('Buildings', 'Moderns', 'Traditionals')
dat$bu2 = dat$name %in% c('Sc_buildings', 'Sc_moderns', 'Sc_traditionals')
dat$bu3 = dat$name %in% c('Buildings', 'Sc_buildings')
dat$other = dat$feature %in% c('Night', 'House')

dat$fold = paste0(dat$train_site, ', ', dat$val_site)


mask = dat$lc1 & dat$radius %in% c(50,100,500)
out = ggplot(dat[mask,],
       aes(x = value, y = average - mts,
           group = interaction(fold, feature),
           color = fold))  + ylab('Effect on trap success prediction') + xlab('Value') + 
    geom_line() + scale_x_continuous(n.breaks = 3) +
    facet_grid2(cols = vars(name), rows = vars(radius), scales = 'free_x',
                             independent = 'x')
out$labels$colour = 'Train site, val site'
out
ggsave('../Figures_agg/pdp_landcover1.png', width = 12, height = 6)


mask = dat$lc2 & dat$radius %in% c(50,100,500)
out = ggplot(dat[mask,],
       aes(x = value, y = average - mts,
           group = interaction(fold, feature),
           color = fold))  + ylab('Effect on trap success prediction') + xlab('Value') + 
    geom_line() + scale_x_continuous(n.breaks = 3) +
    facet_grid2(cols = vars(name), rows = vars(radius), scales = 'free_x',
                             independent = 'x')
out$labels$colour = 'Train site, val site'
out
ggsave('../Figures_agg/pdp_landcover2.png', width = 8, height = 6)

mask = dat$base_name=='Frac_rice'
out = ggplot(dat[mask,],
       aes(x = value, y = average - mts,
           group = interaction(fold, feature),
           color = fold))  + ylab('Effect on trap success prediction') + xlab('Value') + 
    geom_line() + scale_x_continuous(n.breaks = 3) +
    facet_grid2(rows = vars(radius), scales = 'free_x',
                             independent = 'x')
out$labels$colour = 'Train site, val site'
out
ggsave('../Figures_agg/pdp_landcover3.png', width = 7, height = 8)



dat$value[dat$bu1] = dat$value[dat$bu1]*dat$radius[dat$bu1]^2*pi

mask = dat$bu1
out = ggplot(dat[mask,],
       aes(x = value, y = average - mts,
           group = interaction(train_site, val_site, test_site, feature),
           color = fold)) + ylab('Effect on trap success prediction') + xlab('Value') + 
    geom_line() + scale_x_continuous(n.breaks = 3) +
    facet_grid2(cols = vars(name), rows = vars(radius), scales = 'free_x',
                              independent = 'x')
out$labels$colour = 'Train site, val site'
out
ggsave('../Figures_agg/pdp_building1.png', width = 8, height = 8)



mask = dat$bu3 & dat$radius %in% c(25,50,100,200)
out = ggplot(dat[mask,],
       aes(x = value, y = average - mts,
           group = interaction(train_site, val_site, test_site, feature),
           color = fold)) + ylab('Effect on trap success prediction') + xlab('Value') + 
    geom_line() + facet_grid2(cols = vars(name), rows = vars(radius), scales = 'free_x',
                              independent = 'x')
out$labels$colour = 'Train site, val site'
out
ggsave('../Figures_agg/pdp_building2.png', width = 8, height = 8)

mask = dat$other
out = ggplot(dat[mask,],
       aes(x = factor(value), y = average - mts,
           group = interaction(train_site, val_site, test_site, feature))) +
    ylab('Effect on trap success prediction') + xlab('Value') + 
    geom_point(aes(fill = fold), size = 5, alpha = 0.5, shape = 21) +
    geom_line(alpha = 0.5) + #aes(color = fold)) + 
    facet_grid2(cols = vars(name), scales = 'free_x',
                              independent = 'x')
out$labels$colour = 'Train site, val site'
out
ggsave('../Figures_agg/pdp_other.png', width = 8, height = 4)


