library(ggplot2)
library(ggh4x)


model.path = '../Figures_agg_scaled/'



rain.preds <- read.csv(paste0(model.path,'rainfall_preds_train.csv'))
rain.preds$Date <- as.Date(rain.preds$Date)


supp.data <- read.csv('../Data/Rainfall_lag_data/supp_data.csv')
supp.data$Date <- as.Date(supp.data$Date)
All.dat = read.csv('../Data/Trap_Data/Clean_Both_Data_By_Trap.csv')
rod.dat <- data.table::setDT(All.dat)
rod.dat$Date <- as.Date(rod.dat$Date)
rod.dat <- rod.dat[,.(totmn = sum(Trap.weight[Mna==1]), tottraps = sum(Trap.weight),
                      Date = mean(Date)),
        by = list(Visit, Site, House)]
rod.dat$TS <- with(rod.dat, totmn / tottraps)


if(grepl('scaled', model.path)){
    mean.dat = rod.dat[,.(mts = mean(TS)),
                          by = list(Site)]
    rod.dat = merge(rod.dat, mean.dat, by = 'Site', all.x = TRUE, all.y = FALSE)
    rod.dat$TS = rod.dat$TS - rod.dat$mts
}



rain.dat <- c()
for(site in c('Bafodia', 'Bantou', 'Tanganya')){
    rain.temp = read.csv(paste0('../Data/Extracted_Village_Rainfall/', site, '_precip.csv'))
    rain.temp$Site = site
    rain.dat = rbind(rain.dat,
                     rain.temp)
}
rain.dat$Date <- as.Date(paste0(rain.dat$Year, '-', rain.dat$Month, '-', rain.dat$Day))
rain.dat$scPrecip = rain.dat$Precip/100


## --- Plot model predictions, precip, and data for training site

grid.dat <- data.frame(train = c('Bafodia', 'Bantou', 'Tanganya'),
                       val1 = c('Bantou', 'Bafodia', 'Bafodia'),
                       val2 = c('Tanganya', 'Tanganya', 'Bantou'))


## --- train site
for(ii in 1:3){
    site.name = grid.dat$train[ii]
    x.min = min(rod.dat[rod.dat$Site==site.name,]$Date)
    x.max = max(rod.dat[rod.dat$Site==site.name,]$Date)
    mask.rain = rain.dat$Site==site.name
    val1 = grid.dat$val1[grid.dat$train==site.name]
    val2 = grid.dat$val2[grid.dat$train==site.name]
    mask.preds1 = rain.preds$train_site==site.name & rain.preds$val_site==val1
    mask.preds2 = rain.preds$train_site==site.name & rain.preds$val_site==val2
    out = ggplot() +
        geom_smooth(data = rain.dat[mask.rain,],
                    aes(x = Date, y = scPrecip, color = 'Precipitation'),
                    method = 'loess', span = 0.1, se = FALSE,
                    show.legend = TRUE) +
        geom_line(data = rain.preds[mask.preds1 & rain.preds$House==1,],
                  mapping = aes(x = Date, y = pred,
                                group = interaction(train_site, val_site, House),
                                color = "Inside prediction")) +
        geom_line(data = rain.preds[mask.preds1 & rain.preds$House==0,],
                  mapping = aes(x = Date, y = pred,
                                group = interaction(train_site, val_site, House),
                                color = "Outside prediction")) +
        geom_line(data = rain.preds[mask.preds2 & rain.preds$House==1,],
                  mapping = aes(x = Date, y = pred,
                                group = interaction(train_site, val_site, House),
                                color = "Inside prediction")) +
        geom_line(data = rain.preds[mask.preds2 & rain.preds$House==0,],
                  mapping = aes(x = Date, y = pred,
                                group = interaction(train_site, val_site, House),
                                color = "Outside prediction")) +
        geom_point(data = rod.dat[rod.dat$Site==site.name & rod.dat$House==1,],
                   mapping = aes(x = Date, y = TS, size = tottraps, color = "TS-inside",
                                 group = interaction(House))) + 
        geom_point(data = rod.dat[rod.dat$Site==site.name & rod.dat$House==0,],
                   mapping = aes(x = Date, y = TS, size = tottraps, color = "TS-outside",
                                 group = interaction(House))) + 
        scale_color_manual(values = c("Precipitation"="blue","Inside prediction"="orange",
                                      "Outside prediction"="violet", "TS-inside"="orange",
                                      "TS-outside"="violet")) +
        xlim(as.Date(c(x.min, x.max))) + ylab("Trap success")
    out$labels$size = "Total traps"
    out$labels$colour = ""    
    ggsave(filename = paste0(model.path,'timeseries_TS_train_', site.name, '.png'),
           plot = out,
           width = 8, height = 5)
}





rain.preds <- read.csv(paste0(model.path,'rainfall_preds_test.csv'))
rain.preds$Date <- as.Date(rain.preds$Date)

## -- test sites
for(ii in 1:3){
    site.name = grid.dat$train[ii]
    x.min = min(rod.dat[rod.dat$Site==site.name,]$Date)
    x.max = max(rod.dat[rod.dat$Site==site.name,]$Date)
    mask.rain = rain.dat$Site==site.name
    val1 = grid.dat$val1[grid.dat$train==site.name]
    val2 = grid.dat$val2[grid.dat$train==site.name]
    mask.preds1 = rain.preds$test_site==site.name & rain.preds$val_site==val1
    mask.preds2 = rain.preds$test_site==site.name & rain.preds$val_site==val2

    out = ggplot() +
        geom_smooth(data = rain.dat[mask.rain,],
                    aes(x = Date, y = scPrecip, color = 'Precipitation'),
                    method = 'loess', span = 0.1, se = FALSE,
                    show.legend = TRUE) +
        geom_line(data = rain.preds[mask.preds1 & rain.preds$House==1,],
                  mapping = aes(x = Date, y = pred,
                                group = interaction(train_site, val_site, House),
                                color = name)) +
        geom_line(data = rain.preds[mask.preds1 & rain.preds$House==0,],
                  mapping = aes(x = Date, y = pred,
                                group = interaction(train_site, val_site, House),
                                color = "Outside prediction")) +
        geom_line(data = rain.preds[mask.preds2 & rain.preds$House==1,],
                  mapping = aes(x = Date, y = pred,
                                group = interaction(train_site, val_site, House),
                                color = "Inside prediction")) +
        geom_line(data = rain.preds[mask.preds2 & rain.preds$House==0,],
                  mapping = aes(x = Date, y = pred,
                                group = interaction(train_site, val_site, House),
                                color = "Outside prediction")) +
        geom_point(data = rod.dat[rod.dat$Site==site.name & rod.dat$House==1,],
                   mapping = aes(x = Date, y = TS, size = tottraps, color = "TS-inside",
                                 group = interaction(House))) + 
        geom_point(data = rod.dat[rod.dat$Site==site.name & rod.dat$House==0,],
                   mapping = aes(x = Date, y = TS, size = tottraps, color = "TS-outside",
                                 group = interaction(House))) + 
        scale_color_manual(values = c("Precipitation"="blue",'Inside prediction'="orange",
                                      "Outside prediction"="violet", "TS-inside"="orange",
                                      "TS-outside"="violet")) +
        xlim(as.Date(c(x.min, x.max))) + ylab("Trap success")
    out$labels$size = "Total traps"
    out$labels$colour = ""    
    ggsave(filename = paste0(model.path, 'timeseries_TS_test_', site.name, '.png'),
           plot = out,
           width = 8, height = 5)
}


## out = scale_color_manual(values = c("Precipitation"="blue",name="orange",
##                                     "Outside prediction"="violet", "TS-inside"="orange",
##                                     "TS-outside"="violet")) 
