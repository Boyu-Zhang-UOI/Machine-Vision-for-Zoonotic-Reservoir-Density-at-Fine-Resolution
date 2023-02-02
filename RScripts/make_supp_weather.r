### *****************************

## This script constructs a dataframe, named supp.data, that contains
## weather predictors for both sites at 2 week intervals. This is
## used for plotting predictions.

### *****************************

## Load packages for manipulating time series data
require(MASS)
require(lubridate)
require(zoo)
require(reshape)

## Weather lags that are to be incorporated into supp.data
lags <- 1:12

## List of sites to loop through
sites <- c('Bantou', 'Tanganya', 'Bafodia')

## Loop through each site, load in the weather data, and
## calculate the monthly rainfall and temperature lags for each time
## point. Data will be added to the supp.data data frame, which will be
## written to a file.
supp.data <- c()
for(site in sites){

    ## Load weather data files for the specific site
    prec.dat <- read.csv(paste('../Data/Extracted_Village_Rainfall/', site, '_precip.csv',sep = ''))

    ## Add in dates in date form
    prec.dat$Date <- as.Date(paste(prec.dat$Year,prec.dat$Month, prec.dat$Day, sep = '-'))

    ## Insert NA's for any missing (-9999) values
    if(sum(prec.dat$Precip < 0)){'WARNING: NEGATIVE PRECIP VALUES IN PREP_SUPP_WEATHER.r'}
    prec.dat[which(prec.dat$Precip < 0),] <- NA

    ## Build interpolants of rainfall and temperature. These
    ## will be used to easily convert daily rainfall into
    ## monthly averages.
    f.prec <- approxfun(prec.dat$Date, prec.dat$Precip)

    ## Define the dates for which we want predictors in each supp.data file.
    ## Here, we use a 2-week interval. We'll build a monthly interval, then
    ## convert it to biweekly.
    if(site %in% c('Bantou', 'Tanganya')){
        tseq <- seq(as.Date('2002-07-15'), as.Date('2005-5-15'), by = "1 month")
    }else{
        tseq <- seq(as.Date('2019-06-15'), as.Date('2021-05-15'), by = "1 month")
    }
    tseq <- sort(c(tseq, tseq))
    day(tseq) <- c(1,15)
    supp.data.temp <- data.frame(Site = site, Date = tseq)

    ## Loop through each recorded date for the given site, and
    ## calculate rainfall/temp predictors P1:12, and T1:T12.
    for(i in 1:nrow(supp.data.temp)){
        rec.date <- supp.data.temp$Date[i]
        rec.date <- as.Date(rec.date)
        for(lag in lags){
            start.date <- rec.date
            month(start.date) <- month(start.date) - lag
            end.date <- start.date
            month(end.date) <- month(end.date) + 1
            prec.name <- paste('P',lag,sep='')
            int.seq <- seq(start.date, end.date, by = 1)
            supp.data.temp[i, prec.name] <- mean(f.prec(int.seq))
        }
    }
    supp.data <- rbind(supp.data, supp.data.temp)
}

write.csv(supp.data, '../Data/Rainfall_lag_data/supp_data.csv')
