##R Script for: 
##Monitoring climate impact on power reliability in Malawi using satellite data
## Version: 07/10/19
#Any question should be addressed to giacomo.falchetta@feem.it

##############
#############

##This script requires Python 3+, and a number of modules to be installed (reported in the readme.txt file)
library(reticulate)

#Set the Python directory and the conda environment
use_python("C:\\OSGeo4W64\\apps\\Python37", required = T)

#Load required libraries (or install them, previosuly)
library(raster)
library(margins)
library(sf)
library(eia)
library(cowplot)
library(ggsci)
library(gridExtra)
library(googledrive)
library(RColorBrewer)
library(jtools)
library(tidyverse)
library(foreign)
library(frm)
library(sandwich)
library(lmtest)
library(ggpmisc)
library(estimatr)
library(readr)
library(splitstackshape)
library(lfstat)
library(wbstats)
library(Amelia)
library(padr)
library(zoo)
library(hydroGOF)
library(imputeTS)
library(hddtools)
library(lfstat)
library(ggplot2)
library(plotly)
library(reshape2)
library(data.table)
library(tidyverse)
library(stargazer)
library(lubridate)
library(reshape2)
library(readxl)
library(fields)
library(chron)
library(ncdf4)
library(raster)
library(robustbase)
library(rgdal)
library(raster)
Sys.setlocale("LC_TIME", "English")

#Set the working directory
wd = yourworkingdirectory
setwd(wd)

#############
#DATA INPUTTING AND WRANGLING#
############

#1) Lake Malawi: 10-day Status products with datum based on a 9 year (1993-2001) mean
lakeslist<-c("0317")
corresponding_lakename<-c("Lake Malawi")

seg1<-"https://ipad.fas.usda.gov/lakes/images/lake"
seg2<-".TPJOJ.2.smooth.txt"

series_lakeheights<-paste(seg1, lakeslist, seg2, sep="")

daily_fucntion <- function(X){ 
  data.table::fread(X, skip = 11)
} 

import<-lapply(series_lakeheights, daily_fucntion)
names(import)<-corresponding_lakename
lake_levels <- rbindlist(import)
setnames(lake_levels,names(lake_levels),c("yyyymmdd", "hh", "mm", "lakemalawi_level"))

#Remove missing data and restructure dataframe
lake_levels<-lake_levels[!(lake_levels$lakemalawi_level==999.99),]
lake_levels<-data.frame(lake_levels$lakemalawi_level, substr(lake_levels$yyyymmdd, 1, 4), substr(lake_levels$yyyymmdd, 5, 6), substr(lake_levels$yyyymmdd, 7, 8))
setnames(lake_levels,names(lake_levels),c("lakemalawi_level", "year", "month", "day"))
lake_levels$date<-as.Date(paste(lake_levels$year, lake_levels$month, lake_levels$day, sep="-"))

#Interpolate (linearly) 10-day lake level product to daily observations
lake_levels <- dplyr::select(lake_levels, lakemalawi_level, date)
lake_levels$date<-as.POSIXct(lake_levels$date,format="%Y-%m-%d")
lake_levels = pad(lake_levels)
lake_levels <- zoo(lake_levels$lakemalawi_level, lake_levels$date)
lake_levels<- na.interpolation(lake_levels, option = "linear")
lake_levels = fortify.zoo(lake_levels)

#Define data variables
lake_levels$year=year(lake_levels$Index)
lake_levels$month=month(lake_levels$Index)
lake_levels$day=day(lake_levels$Index)

#2) Import historical data for precipitations (CHIRPS) over the entire Shire river basin
source_python("Earth_engine_CHIRPS_2010_2018.py")
chirps = read.csv("chirps_rainfall_malawi_2000_2010.csv")

#Restructure data and add date variables
chirps = dplyr::select(chirps, matches("X2|name"))
chirps$id=1
chirps_long = merged.stack(chirps, var.stubs = "X", sep = "var.stubs")
varnames<-c("name", "date", "precipitations_chirps")
setnames(chirps_long,names(chirps_long),varnames )
chirps_long <- dplyr::select(chirps_long, precipitations_chirps, date)
chirps_long$date<-as.POSIXct(chirps_long$date,format="%Y%m%d")
chirps_long$year=year(chirps_long$date)
chirps_long$month=month(chirps_long$date)
chirps_long$day=day(chirps_long$date)


# 

chirps = read.csv("chirps_rainfall_malawi_2010_2018.csv")

#Restructure data and add date variables
chirps = dplyr::select(chirps, matches("X2|name"))
chirps$id=1
chirps_long_2 = merged.stack(chirps, var.stubs = "X", sep = "var.stubs")
varnames<-c("name", "date", "precipitations_chirps")
setnames(chirps_long_2,names(chirps_long_2),varnames )
chirps_long_2 <- dplyr::select(chirps_long_2, precipitations_chirps, date)
chirps_long_2$date<-as.POSIXct(chirps_long_2$date,format="%Y%m%d")
chirps_long_2$year=year(chirps_long_2$date)
chirps_long_2$month=month(chirps_long_2$date)
chirps_long_2$day=day(chirps_long_2$date)

chirps_long = rbind(chirps_long, chirps_long_2)

#3) Import precipitation anomalies over malawi
source_python("moisture.py")
moisture = read.csv("moisture.csv")
moisture = dplyr::select(moisture, matches("X|NASA"))

#Restructure data, perform linear interpolation, and add date variables
moisture_long = merged.stack(moisture, var.stubs = "NASA_USDA_SM", sep = "var.stubs")
varnames<-c("X", "date", "moisture_NASA")
setnames(moisture_long,names(moisture_long),varnames )
moisture_long <- as.data.frame(dplyr::select(moisture_long, moisture_NASA, date))
moisture_long$date = as.Date(substr(moisture_long$date, 1, 8), format="%Y%m%d")

moisture_long = dplyr::mutate(moisture_long, date = as.Date(date))
moisture_long = complete(moisture_long, date = seq.Date(min(date), max(date), by="day"))

moisture_long <- zoo(moisture_long$moisture_NASA, moisture_long$date)
moisture_long<- na.interpolation(moisture_long, option = "linear")
moisture_long = fortify.zoo(moisture_long)

moisture_long$year=year(moisture_long$Index)
moisture_long$month=month(moisture_long$Index)
moisture_long$day=day(moisture_long$Index)

#4) Import surface air temperature over Malawi
source_python("Earth_engine_temperature.py")

temperature = read.csv("average_temperature_shirebasin_2000_2010.csv")

#Restructure data, perform linear interpolation, and add date variables
temperature$id=1
temperature = merged.stack(temperature, var.stubs = "X", sep = "var.stubs")
temperature = select(temperature, X, .time_1)
varnames = c("temp", "date")
setnames(temperature,names(temperature),varnames )
temperature$date<-as.POSIXct(temperature$date,format="%Y_%m_%d")
temperature = temperature[-1,]
temperature <- zoo(temperature$temp, temperature$date)
temperature<- na.interpolation(temperature, option = "linear")
temperature = fortify.zoo(temperature)

temperature$year=year(temperature$Index)
temperature$month=month(temperature$Index)
temperature$day=day(temperature$Index)


temperature_2 = read.csv("average_temperature_shirebasin_2010_2018.csv")

#Restructure data, perform linear interpolation, and add date variables
temperature_2$id=1
temperature_2 = merged.stack(temperature_2, var.stubs = "X", sep = "var.stubs")
temperature_2 = select(temperature_2, X, .time_1)
varnames = c("temp", "date")
setnames(temperature_2,names(temperature_2),varnames )
temperature_2$date<-as.POSIXct(temperature_2$date,format="%Y_%m_%d")
temperature_2 = temperature_2[-1,]
temperature_2 <- zoo(temperature_2$temp, temperature_2$date)
temperature_2<- na.interpolation(temperature_2, option = "linear")
temperature_2 = fortify.zoo(temperature_2)

temperature_2$year=year(temperature_2$Index)
temperature_2$month=month(temperature_2$Index)
temperature_2$day=day(temperature_2$Index)
temperature_2 = rename(temperature_2, temperature = temperature_2)
  
temperature = rbind(temperature, temperature_2)


#5) Import discharge and water level at 4 gauging stations 
#2017 data
shire_2017 <- read_excel("Shire river\\ShireRiver Flow Data\\2017-2018 shire river flow data all stations.xls")
shire_2017$Date<-as.Date(shire_2017$Date,format='%Y\\%m\\%d')
shire_2017_long <- melt(shire_2017, id = "Date")
shire_2017_long_2<-shire_2017_long[grep("dis", shire_2017_long$variable), ]

#1970-2015 data
shire_1970 <- read_excel("Shire river\\ShireRiver Flow Data\\Shire at Chikwawa, Liwonde and Matope.xls", col_types = c("date", "numeric", "numeric", "numeric"))

shire_1970$date<-as.Date(shire_1970$date,format='%Y\\%m\\%d')
shire_1970_long <- melt(shire_1970, id = "date")

##i987-1998 data at Zalewa
wd = getwd()
setwd(paste0(wd, "/Shire river/ShireRiver Flow Data/Zalewa"))

fun<-function(X){
  data<-readr::read_tsv(X)
  data<-data %>%
    dplyr::select(-X2, -X15) %>%
    dplyr::rename(day = X1) 
  data<-data %>%
    tidyr::gather(key = month, value = value, -day)
}

Zalewa<-lapply(list.files(pattern = "\\.txt$"), fun)
names(Zalewa)<-c(list.files(pattern = "\\.txt$"))

Zalewa<-rbindlist(Zalewa, idcol=TRUE)
Zalewa<-na.omit(Zalewa)

Zalewa$year<-1

Zalewa[1:61,]$year<-1987
Zalewa[62:427,]$year<-1988
Zalewa[428:792,]$year<-1989
Zalewa[792:1157,]$year<-1990
Zalewa[1158:1522,]$year<-1991
Zalewa[1523:1888,]$year<-1992
Zalewa[1889:2253,]$year<-1993
Zalewa[2254:2618,]$year<-1994
Zalewa[2619:2983,]$year<-1995
Zalewa[2984:3349,]$year<-1996
Zalewa[3350:3714,]$year<-1997
Zalewa[3715:4018,]$year<-1998

Zalewa$month<-match(Zalewa$month,month.abb)
Zalewa$date<-as.Date(paste(Zalewa$day, Zalewa$month, Zalewa$year, sep = "\\"),format='%d\\%m\\%Y')

##2017-18 data at Liwonde
daily_fucntion <- function(X){
  data.table::fread(X, skip = 4)
} 

daily<-daily_fucntion(paste0(wd, "\\Shire river\\ShireRiver Flow Data\\Liwonde.txt"))
Liwonde_201718<-data.frame(substr(daily$V1, 1, 10), daily$V2)
colnames(Liwonde_201718)<-c("date", "discharge")
Liwonde_201718$date<-as.Date(Liwonde_201718$date,format='%d.%m.%Y')

#Merge
write.csv(Liwonde_201718, "Liwonde_201718.csv")
write.csv(Zalewa, "Zalewa.csv")
write.csv(shire_2017_long_2, "shire_2017_long_2.csv")
write.csv(shire_1970_long, "shire_1970_long.csv")

discharge_shire<-read.csv("shire_1970_long.csv")
discharge_shire$date<-as.Date(discharge_shire$date,format='%Y-%m-%d')
dis<-discharge_shire[discharge_shire$date > as.Date("1970-01-01"),]

discharge_shire<-read.csv("discharge_shire.csv")

mmm<-function(X){
  max(X, na.rm=TRUE)
}

jj_spread <- dcast(discharge_shire, Date ~ variable, value.var="value", fun.aggregate=mmm)
is.na(jj_spread) <- jj_spread == ("-Inf")
river_discharge_stations <- jj_spread[-1, ]

river_discharge_stations$Date = as.Date(river_discharge_stations$Date, format="%d/%m/%Y")
names(river_discharge_stations)[names(river_discharge_stations) == 'Date'] <- 'date'

river_discharge_stations$year=year(river_discharge_stations$date)
river_discharge_stations$month=month(river_discharge_stations$date)
river_discharge_stations$day=day(river_discharge_stations$date)


#6) Import generation at hydropower schemes from EGENCO
setwd(paste0(wd, "\\EGENCO data"))

sheets <- readxl::excel_sheets("EGENCO Daily Generation Data.xlsx")
lst <- lapply(sheets[1:7], function(sheet) 
  readxl::read_excel("EGENCO Daily Generation Data.xlsx", sheet = sheet, col_types=c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
)
names(lst) <- sheets[1:7]

egencodata = do.call(rbind, lst)

egencodata$Date = as.Date(egencodata$Date, format = "%Y-%m-%d")
egencodata = egencodata[complete.cases(egencodata[ , 2:6]),]   

#Total generation variables
egencodata$total=rowSums(egencodata[2:7], na.rm = TRUE)

#Date variables
egencodata$year=year(egencodata$Date)
egencodata$month=month(egencodata$Date)
egencodata$day=day(egencodata$Date)

#7) Import drought database  data(SPEI index)
#7.1. import spei, shapefile, and extract over Shire river basin
#try different scales and see which correlates better with lake level and discharge
setwd(wd)

#03
ncfile03 <- "spei03.nc"
spei03 <- brick(ncfile03)

shire_basin<-readOGR("Shire river\\shirebasin\\shirebasin.shp")
spei03_malawi<-raster::extract(spei03, shire_basin, fun=mean, na.rm=TRUE)

spei03_malawi<-melt(spei03_malawi,measure.vars=names(spei03_malawi)[2:length(names(spei03_malawi))], id.vars ="id")
spei03_malawi$date<-substr(spei03_malawi$Var2,2,11)
spei03_malawi$date = as.Date(spei03_malawi$date, format="%Y.%m.%d")

varnames<-c("var1", "var2", "spei03", "date")
setnames(spei03_malawi,names(spei03_malawi),varnames )

spei03_malawi <- spei03_malawi %>% dplyr::select(spei03, date)

#6
ncfile06 <- "spei06.nc"
spei06 <- brick(ncfile06)

spei06_malawi<-raster::extract(spei06, shire_basin, fun=mean, na.rm=TRUE)

spei06_malawi<-melt(spei06_malawi,measure.vars=names(spei06_malawi)[2:length(names(spei06_malawi))], id.vars ="id")
spei06_malawi$date<-substr(spei06_malawi$Var2,2,11)
spei06_malawi$date = as.Date(spei06_malawi$date, format="%Y.%m.%d")

varnames<-c("var1", "var2", "spei06", "date")
setnames(spei06_malawi,names(spei06_malawi),varnames )

spei06_malawi <- spei06_malawi %>% dplyr::select(spei06, date)

#12
ncfile12 <- "spei12.nc"
spei12 <- brick(ncfile12)

spei12_malawi<-raster::extract(spei12, shire_basin, fun=mean, na.rm=TRUE)

spei12_malawi<-melt(spei12_malawi,measure.vars=names(spei12_malawi)[2:length(names(spei12_malawi))], id.vars ="id")
spei12_malawi$date<-substr(spei12_malawi$Var2,2,11)
spei12_malawi$date = as.Date(spei12_malawi$date, format="%Y.%m.%d")

varnames<-c("var1", "var2", "spei12", "date")
setnames(spei12_malawi,names(spei12_malawi),varnames )

spei12_malawi <- spei12_malawi %>% dplyr::select(spei12, date)

#24
ncfile24 <- "spei24.nc"
spei24 <- brick(ncfile24)

spei24_malawi<-raster::extract(spei24, shire_basin, fun=mean, na.rm=TRUE)

spei24_malawi<-melt(spei24_malawi,measure.vars=names(spei24_malawi)[2:length(names(spei24_malawi))], id.vars ="id")
spei24_malawi$date<-substr(spei24_malawi$Var2,2,11)
spei24_malawi$date = as.Date(spei24_malawi$date, format="%Y.%m.%d")

varnames<-c("var1", "var2", "spei24", "date")
setnames(spei24_malawi,names(spei24_malawi),varnames )

spei24_malawi <- spei24_malawi %>% dplyr::select(spei24, date)

#47
ncfile47 <- "spei47.nc"
spei47 <- brick(ncfile47)

spei47_malawi<-raster::extract(spei47, shire_basin, fun=mean, na.rm=TRUE)

spei47_malawi<-melt(spei47_malawi,measure.vars=names(spei47_malawi)[2:length(names(spei47_malawi))], id.vars ="id")
spei47_malawi$date<-substr(spei47_malawi$Var2,2,11)
spei47_malawi$date = as.Date(spei47_malawi$date, format="%Y.%m.%d")

varnames<-c("var1", "var2", "spei47", "date")
setnames(spei47_malawi,names(spei47_malawi),varnames )

spei47_malawi <- spei47_malawi %>% dplyr::select(spei47, date)

#Merge SPEI at all scales
spei_malawi = Reduce(function(x,y) merge(x,y,by="date",all=TRUE) ,list(spei03_malawi, spei06_malawi, spei12_malawi, spei24_malawi, spei47_malawi))
spei_malawi$year=year(spei_malawi$date)
spei_malawi = subset(spei_malawi, spei_malawi$year > 1969)
spei_malawi$date<-as.POSIXct(spei_malawi$date,format="%Y-%m-%d")

#Daily linear interpolation of SPEI monthly values
spei_malawi = pad(spei_malawi)
spei_malawi<- na.interpolation(spei_malawi, option = "linear")
spei_malawi = fortify.zoo(spei_malawi)

spei_malawi$year=year(spei_malawi$date)
spei_malawi$month=month(spei_malawi$date)
spei_malawi$day=day(spei_malawi$date)


#8) Merge all hitherto imported dailydata
daily = Reduce(function(x,y) merge(x,y,by=c("day", "month", "year"),all=TRUE) ,list(temperature, egencodata, spei_malawi, river_discharge_stations, moisture_long, chirps_long, lake_levels))
daily <- subset( daily, select = -c(Index.x, Index.y, date, Date, date.x, date.y ) )
daily = data.table(daily)

#Date variables
daily$date=as.Date(paste0(daily$year, "-", daily$month, "-", daily$day), format="%Y-%m-%d")
daily$month=month(daily$date)
daily$year=year(daily$date)
daily$day=day(daily$date)

#Ordering
daily = daily[order(year, month, day)]

#############
#MODELLING#
############

#1) Create a function to create lags and leads for time-series regression analysis
shift<-function(x,shift_by){
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))
  
  if (length(shift_by)>1)
    return(sapply(shift_by,shift, x=x))
  
  out<-NULL
  abs_shift_by=abs(shift_by)
  if (shift_by > 0 )
    out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
  else if (shift_by < 0 )
    out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
  else
    out<-x
  out
}

#Create lags and rolling sum/mean variables
daily$lastt3monthsprec<- rollsumr(daily$precipitations_chirps, k = 90, fill = NA)
daily$last3monthtemp<- rollmeanr(daily$temperature, k = 90, fill = NA)

####
library(caret)

# Select relevant variables
dailyml = daily %>%  drop_na("last3monthtemp" , "spei03" , "spei06" , "spei12" , "spei24" , "spei47" , "lastt3monthsprec") %>% as.data.frame()

dailyml_1 = dailyml %>% dplyr::select(lake_levels, month, last3monthtemp, spei03, spei06, spei12, spei24, spei47, lastt3monthsprec) %>% as.data.frame()

# Partition data
splitSample <- sample(1:2, size=nrow(dailyml_1), prob=c(0.7,0.3), replace = TRUE)
train.hex <- dailyml_1[splitSample==1,]
test.hex <- dailyml_1[splitSample==2,]

# Enable multithread support
library(doParallel)
#cores_2_use <- floor(0.8*detectCores())
cores_2_use <- 3
cl <- makeCluster(cores_2_use, outfile = "parallel_log.txt")
registerDoParallel(cl)

# Model cross validations options
fitControl <- trainControl(
  ## Repeated 5-fold CV 
  method = "cv",
  number = 10,
  verboseIter = TRUE,
  returnResamp = "all", allowParallel = TRUE)

# definisci manualmente i parameter (num.trees, mtry, splitrule) space con tunematrix
rrfFit <- train(lake_levels ~ ., 
                data = train.hex,
                method = 'ranger',
                # should be set high at least p/3
                tuneLength = 10,
                trControl = fitControl,
                num.trees=250,
                importance = "permutation", na.action = na.omit)

# Print CV accuracy
print(rrfFit)

# Variable importance
varImp(rrfFit)

# Predict on test
test.hex$lake_levels_forecasted = (predict(rrfFit, test.hex))

# R2 for test (= test accuracy)
formula<-"lake_levels ~ lake_levels_forecasted"
ols1<-lm(formula,data=test.hex)
summary(ols1, robust=TRUE)  

# Predict all
dailyml$lake_levels_forecasted = predict(rrfFit, newdata=dailyml)

# Shut down parallel
stopCluster(cl)
registerDoSEQ()

###

# Plots
png("reg1.png")
trellis.par.set(caretTheme())
plot(rrfFit)
dev.off()


## variable importance
rfImp <- varImp(rrfFit)
rfImp
png("reg1b.png")
plot(rfImp,top = 8)
dev.off()

# Plots 
#densityplot(rrfFit,
 #           adjust = 1.25)

#xyplot(rrfFit)


####
#Plot predicted vs. real with abline
formula <- y ~ x

lake = ggplot(dailyml, aes(y=lake_levels, x=lake_levels_forecasted, colour=month))+
  geom_point(size=2)+
  theme_grey()+
  xlab('Predicted (m)')+
  ylab('Measured (m)')+
  #ggtitle('04/2012 - 08/2018, predicting Lake Malawi with SPEI, \n cumul. precip, av.temp, and month \n over the Shire River Basin')+
  scale_color_continuous(name="Month")+
  scale_y_continuous(limits = c(-2, 0.5))+
  scale_x_continuous(limits = c(-2, 0.5))+
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formula, parse = TRUE, size = 6, rr.digits = 4)

#Plot TS of predicted vs. real 
lake_compared = ggplot()+
  theme_grey()+
  geom_line(data=dailyml, aes(y=lake_levels, x=date, colour="Actual"))+
  geom_line(data=dailyml, aes(y=lake_levels_forecasted, x=date, colour="Predicted"))+
  xlab('Year')+
  ylab('Water level deviation \n at Lake Malawi (m)')+
  scale_colour_discrete(name="Legend")

p <- plot_grid(lake_compared, lake, labels=c('A', 'B'))
title <- ggdraw() + draw_label("Water level at Lake Malawi", fontface='bold')
lake_combined = plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

ggsave(plot=lake_combined, device = "png", filename = "lake_combined.png", width = 12, height = 5, scale = 0.8)


#3) Predict discharge at various stations on the Shire River using Lake Level and Climate Variables
#Liwonde
# Select relevant variables
dailyml_1 = dailyml %>%  drop_na("Liwonde_dis", "month", "spei03" , "spei06" , "spei12" , "spei24" , "spei47", "lastt3monthsprec", "lake_levels_forecasted", "lastt3monthsprec") %>% as.data.frame()

dailyml_1 = dailyml_1 %>% dplyr::select(lake_levels_forecasted, month, temperature, spei03, spei06, spei12, spei24, spei47, lastt3monthsprec, last3monthtemp, Liwonde_dis) %>% as.data.frame()

# Partition data
splitSample <- sample(1:2, size=nrow(dailyml_1), prob=c(0.7,0.3), replace = TRUE)
train.hex <- dailyml_1[splitSample==1,]
test.hex <- dailyml_1[splitSample==2,]

# Enable multithread support
library(doParallel)
cores_2_use <- 3
cl <- makeCluster(cores_2_use, outfile = "parallel_log.txt")
registerDoParallel(cl)

# Model cross validations options
fitControl <- trainControl(
  ## Repeated 5-fold CV 
  method = "cv",
  number = 10,
  verboseIter = TRUE,
  returnResamp = "all", allowParallel = TRUE)

# definisci manualmente i parameter (num.trees, mtry, splitrule) space con tunematrix

rrfFit <- train(Liwonde_dis ~ ., 
                data = train.hex,
                method = 'ranger',
                tuneLength = 10,
                trControl = fitControl,
                num.trees=250,
                importance = "permutation", na.action = na.omit)

# Plots
png("reg2.png")
trellis.par.set(caretTheme())
plot(rrfFit)
dev.off()


## variable importance
rfImp <- varImp(rrfFit)
rfImp
png("reg2b.png")
plot(rfImp,top = 10)
dev.off()


# Print CV accuracy
print(rrfFit)
print(rrfFit$resample)

# Predict on test
test.hex$Liwonde_dis_forecasted = (predict(rrfFit, test.hex))

# R2 for test (= test accuracy)
formula<-"Liwonde_dis ~ Liwonde_dis_forecasted"
ols1<-lm(formula,data=test.hex)
summary(ols1, robust=TRUE)  

# Predict all
dailyml$Liwonde_dis_forecasted = (predict(rrfFit, dailyml, type="raw"))

# Shut down parallel
stopCluster(cl)
registerDoSEQ()

#Matope and Zalewa
daily$MatZaw_dis=ifelse(is.na(daily$Matope_dis) & !is.na(daily$Zalewa_dis), daily$Zalewa_dis, ifelse(!is.na(daily$Matope_dis) & is.na(daily$Zalewa_dis), daily$Matope_dis, ifelse(!is.na(daily$Matope_dis) & !is.na(daily$Zalewa_dis), (daily$Matope_dis + daily$Zalewa_dis)/2, NA)))

# Select relevant variables
dailyml_1 = dailyml %>%  drop_na("Liwonde_dis_forecasted", "MatZaw_dis", "month", "spei03" , "spei06" , "spei12" , "spei24" , "spei47", "lastt3monthsprec", "lake_levels_forecasted", "lastt3monthsprec") %>% as.data.frame()

dailyml_1 = dailyml_1 %>% dplyr::select(lake_levels_forecasted, month, temperature, spei03, spei06, spei12, spei24, spei47, lastt3monthsprec, last3monthtemp, Liwonde_dis_forecasted, MatZaw_dis) %>% as.data.frame()

# Partition data
splitSample <- sample(1:2, size=nrow(dailyml_1), prob=c(0.7,0.3), replace = TRUE)
train.hex <- dailyml_1[splitSample==1,]
test.hex <- dailyml_1[splitSample==2,]

# Enable multithread support
cores_2_use <- 3
cl <- makeCluster(cores_2_use, outfile = "parallel_log.txt")
registerDoParallel(cl)

# Model cross validations options
fitControl <- trainControl(
  ## Repeated 5-fold CV 
  method = "cv",
  number = 10,
  verboseIter = TRUE,
  returnResamp = "all", allowParallel = TRUE)

# definisci manualmente i parameter (num.trees, mtry, splitrule) space con tunematrix

rrfFit <- train(MatZaw_dis ~ ., 
                data = train.hex,
                method = 'ranger',
                tuneLength = 10,
                trControl = fitControl,
                num.trees=250,
                importance = "permutation", na.action = na.omit)

# Plots
png("reg3.png")
trellis.par.set(caretTheme())
plot(rrfFit)
dev.off()


## variable importance
rfImp <- varImp(rrfFit)
rfImp
png("reg3b.png")
plot(rfImp,top = 10)
dev.off()


# Print CV accuracy
print(rrfFit)
print(rrfFit$resample)

# Predict on test
test.hex$MatZaw_dis_forecasted = (predict(rrfFit, test.hex))

# R2 for test (= test accuracy)
formula<-"MatZaw_dis ~ MatZaw_dis_forecasted"
ols1<-lm(formula,data=test.hex)
summary(ols1, robust=TRUE)  

# Predict all
dailyml$MatZaw_dis_forecasted = (predict(rrfFit, dailyml))

# Shut down parallel
stopCluster(cl)
registerDoSEQ()

###

# Select relevant variables
dailyml_1 = dailyml %>%  drop_na("Liwonde_dis_forecasted", "MatZaw_dis_forecasted", "Chikwawa_dis" , "month", "spei03" , "spei06" , "spei12" , "spei24" , "spei47", "lastt3monthsprec", "lake_levels_forecasted", "lastt3monthsprec") %>% as.data.frame()

dailyml_1 = dailyml_1 %>% dplyr::select(lake_levels_forecasted, month, temperature, spei03, spei06, spei12, spei24, spei47, lastt3monthsprec, last3monthtemp, Liwonde_dis_forecasted, MatZaw_dis_forecasted, Chikwawa_dis) %>% as.data.frame()

# Partition data
splitSample <- sample(1:2, size=nrow(dailyml_1), prob=c(0.7,0.3), replace = TRUE)
train.hex <- dailyml_1[splitSample==1,]
test.hex <- dailyml_1[splitSample==2,]

# Enable multithread support
cores_2_use <- 3
cl <- makeCluster(cores_2_use, outfile = "parallel_log.txt")
registerDoParallel(cl)

# Model cross validations options
fitControl <- trainControl(
  ## Repeated 5-fold CV 
  method = "cv",
  number = 10,
  verboseIter = TRUE,
  returnResamp = "all", allowParallel = TRUE)

# definisci manualmente i parameter (num.trees, mtry, splitrule) space con tunematrix

rrfFit <- train(Chikwawa_dis ~ ., 
                data = train.hex,
                method = 'ranger',
                tuneLength = 10,
                trControl = fitControl,
                num.trees=250,
                importance = "permutation", na.action = na.omit)

# Plots
png("reg4.png")
trellis.par.set(caretTheme())
plot(rrfFit)
dev.off()


## variable importance
rfImp <- varImp(rrfFit)
rfImp
png("reg4b.png")
plot(rfImp,top = 10)
dev.off()


# Print CV accuracy
print(rrfFit)
print(rrfFit$resample)

# Predict on test
test.hex$Chikwawa_dis_forecasted = (predict(rrfFit, test.hex))

# R2 for test (= test accuracy)
formula<-"Chikwawa_dis ~ Chikwawa_dis_forecasted"
ols1<-lm(formula,data=test.hex)
summary(ols1, robust=TRUE)  

# Predict all
dailyml$Chikwawa_dis_forecasted = (predict(rrfFit, dailyml, type="raw"))

# Shut down parallel
stopCluster(cl)
registerDoSEQ()

#Better prediction: validate by means of Nash-Sutcliffe Efficiency and Kling-Gupta Efficiency
#Generate dataframes of simulated and observed
simulated = dailyml$Liwonde_dis_forecasted
observed = dailyml$Liwonde_dis

NSE(simulated, observed, na.rm=TRUE, FUN=log, epsilon="other", epsilon.value=1)
KGE(simulated, observed, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012"), out.type=c("single", "full"))

simulated = dailyml$MatZaw_dis_forecasted
observed = dailyml$MatZaw_dis

NSE(simulated, observed, na.rm=TRUE, FUN=log, epsilon="other", epsilon.value=1)
KGE(simulated, observed, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012"), out.type=c("single", "full"))

simulated = dailyml$Chikwawa_dis_forecasted
observed = dailyml$Chikwawa_dis

NSE(simulated, observed, na.rm=TRUE, FUN=log, epsilon="other", epsilon.value=1)
KGE(simulated, observed, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012"), out.type=c("single", "full"))

#Produce an abline-type validation plot for discharge at Liwonde and Matope
formula <- y ~ x

river_liw = ggplot(dailyml, aes(y=Liwonde_dis, x=Liwonde_dis_forecasted, colour=month))+
  theme_grey()+
  geom_point(size=2)+
  geom_abline()+
  xlab(expression(paste("Predicted (", m^3/s, ")", sep = "")))+
  ylab(expression(paste("Measured (", m^3/s, ")", sep = "")))+
  #ggtitle('07/2010 - 08/2018, predicting discharge at Liwonde \n with Lake level, SPEI, cumul. precip, and month')+
  scale_color_continuous(name="Month")+
  scale_y_continuous(limits = c(0, 450))+
  scale_x_continuous(limits = c(0, 450))+
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formula, parse = TRUE, size = 5, rr.digits = 4)

#Plot TS of predicted vs. real 
river_liw_compared = ggplot()+
  theme_grey()+
  geom_line(data=dailyml, aes(y=Liwonde_dis, x=date, colour="Actual"))+
  geom_line(data=dailyml, aes(y=Liwonde_dis_forecasted, x=date, colour="Predicted"))+
  xlab('Year')+
  ylab('Discharge at Liwonde \n (Shire River)')+
  scale_colour_discrete(name="Legend")

ggsave(plot=river_liw, device = "png", filename = "river_liw_predicted.png", width = 6, height = 4, scale = 1)
ggsave(plot=river_liw_compared, device = "png", filename = "river_liw_compared.png", width = 6, height = 4, scale = 1)

river_mz = ggplot(dailyml, aes(y=MatZaw_dis, x=MatZaw_dis_forecasted, colour=month))+
  theme_grey()+
  geom_point(size=2)+
  geom_abline()+
  xlab(expression(paste("Predicted (", m^3/s, ")", sep = "")))+
  ylab(expression(paste("Measured (", m^3/s, ")", sep = "")))+
  #ggtitle('07/2010 - 08/2018, predicting discharge at Liwonde \n with Lake level, SPEI, cumul. precip, and month')+
  scale_color_continuous(name="Month")+
  scale_y_continuous(limits = c(100, 250))+
  scale_x_continuous(limits = c(100, 250))+
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formula, parse = TRUE, size = 5, rr.digits = 4)

#Plot TS of predicted vs. real 
river_mz_compared = ggplot()+
  theme_grey()+
  geom_line(data=dailyml, aes(y=MatZaw_dis, x=date, colour="Actual"))+
  geom_line(data=dailyml, aes(y=MatZaw_dis_forecasted, x=date, colour="Predicted"))+
  xlab('Year')+
  ylab('Discharge at Zalewa \n (Shire River)')+
  scale_colour_discrete(name="Legend")

ggsave(plot=river_mz, device = "png", filename = "river_mz_predicted.png", width = 6, height = 4, scale = 1)
ggsave(plot=river_mz_compared, device = "png", filename = "river_mz_compared.png", width = 6, height = 4, scale = 1)

river_chi = ggplot(dailyml, aes(y=Chikwawa_dis, x=Chikwawa_dis_forecasted, colour=month))+
  theme_grey()+
  geom_point(size=2)+
  geom_abline()+
  xlab(expression(paste("Predicted (", m^3/s, ")", sep = "")))+
  ylab(expression(paste("Measured (", m^3/s, ")", sep = "")))+
  #ggtitle('07/2010 - 08/2018, predicting discharge at Liwonde \n with Lake level, SPEI, cumul. precip, and month')+
  scale_color_continuous(name="Month")+
  scale_y_continuous(limits = c(500, 700))+
  scale_x_continuous(limits = c(500, 700))+
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formula, parse = TRUE, size = 5, rr.digits = 4)

#Plot TS of predicted vs. real 
river_chi_compared = ggplot()+
  theme_grey()+
  geom_line(data=dailyml, aes(y=Chikwawa_dis, x=date, colour="Actual"))+
  geom_line(data=dailyml, aes(y=Chikwawa_dis_forecasted, x=date, colour="Predicted"))+
  xlab('Year')+
  ylab('Discharge at Chikwawa \n (Shire River)')+
  scale_colour_discrete(name="Legend")

ggsave(plot=river_chi, device = "png", filename = "river_chi_predicted.png", width = 6, height = 4, scale = 1)
ggsave(plot=river_chi_compared, device = "png", filename = "river_chi_compared.png", width = 6, height = 4, scale = 1)

p <- plot_grid(river_liw_compared, river_liw, river_mz_compared, river_mz, river_chi_compared, river_chi, labels=c('A', 'B', 'C', 'D', 'E', 'F'), nrow = 3, ncol = 2)
title <- ggdraw() + draw_label("Discharge at the Shire River", fontface='bold')
river_combined = plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

ggsave(plot=river_combined, device = "png", filename = "river_combined.png", width = 26, height = 28, scale = 0.3)

#4) Import monthly capcity in operation at each plant
# bring estimated discharge into 'daily'
inoperationcapacity <- read_excel("Other data/inoperationcapacity.xlsx")
daily2=merge(dailyml, inoperationcapacity, by=c("month", "year"), all=TRUE)

#Calculate total capacity in operation and the total hydropower capacity factor
daily2$inoperationcapacity=rowSums(select(daily2, oc_Kapichira, `oc_Nkula A`, `oc_Nkula B`, oc_Tedzani))
daily2$capfactor =  ((daily2$total/1000)/(daily2$inoperationcapacity*24))

#Calculate capacity factor at indivdual schemes
daily2$cf_nkula =  (((daily2$`Nkula A`+daily2$`Nkula B`)/1000)/((daily2$`oc_Nkula A`+daily2$`oc_Nkula B`)*24))
daily2$cf_tedzani =  (((daily2$`Tedzani I&II`+daily2$`Tedzani III`)/1000)/((daily2$oc_Tedzani)*24))
daily2$cf_kapichira =  ((daily2$Kapichira/1000)/((daily2$oc_Kapichira)*24))

#Define DD, a measure of absolute value of deviation discharge from the long-run mean discharge
daily2= daily2 %>%  group_by(month) %>% mutate(deviation_discharge= MatZaw_dis_forecasted -(mean(MatZaw_dis_forecasted, na.rm=TRUE))) %>% ungroup()

# Data quality situation

tre <- ggplot()+
  xlab("Date")+
  ylab("Discharge (m3/s)")+
  ggtitle("Chikwawa")+
  geom_line(data=daily, aes(x=date, y=Chikwawa_dis), colour="darkred")+
  geom_line(data=daily, aes(x=date, y=MatZaw_dis), colour="navyblue")+
  geom_line(data=daily, aes(x=date, y=Liwonde_dis), colour="forestgreen")

ggsave(tre, filename="dqs.png", device="png", scale=0.5, width = 30, height = 15)


#5) Assess the effect of deviations in the discharge of hydropower capacity factor
#With generalised linear models with logit link to account for the response variable being fractional

##DD Calculated at Liwonde
##

daily2= daily2 %>% filter(capfactor<1)


library (betareg)
betaMod1 <- betareg(capfactor ~  deviation_discharge, data = daily2) # train model. Tune 
summary (betaMod1) 
summary(margins(betaMod1))

betaMod2 <- betareg(capfactor ~  deviation_discharge + as.factor(month), data = daily2) # train model. Tune 
summary (betaMod2) 
summary(margins(betaMod2))

stargazer(betaMod1, betaMod2, type = "latex", dep.var.labels   = "Total monthly HCF", add.lines = list(c("Month fixed effects", "No", "Yes")))

#Regression for indivdual schemes' capacity factors
daily2$cf_nkula = ifelse(daily2$cf_nkula>=1, 0.99, daily2$cf_nkula)
daily2$cf_nkula = ifelse(daily2$cf_nkula==0, 0.0001, daily2$cf_nkula)

betaMod1 <- betareg(cf_nkula ~  deviation_discharge, data = daily2) # train model. Tune 
summary (betaMod1) 
summary(margins(betaMod1))

betaMod2 <- betareg(cf_nkula ~  deviation_discharge + factor(month), data = daily2) # train model. Tune 
summary (betaMod2) 
summary(margins(betaMod2))

stargazer(betaMod1, betaMod2, type = "latex", dep.var.labels   = "Monthly HCF at Nkula", add.lines = list(c("Month fixed effects", "No", "Yes")))

daily2$cf_kapichira = ifelse(daily2$cf_kapichira>=1, 0.99, daily2$cf_kapichira)
daily2$cf_kapichira = ifelse(daily2$cf_kapichira==0, 0.0001, daily2$cf_kapichira)

betaMod1 <- betareg(cf_kapichira ~  deviation_discharge, data = daily2) 
summary (betaMod1) 
summary(margins(betaMod1))

betaMod2 <- betareg(cf_kapichira ~  deviation_discharge + factor(month), data = daily2) 
summary (betaMod2) 
summary(margins(betaMod2))

stargazer(betaMod1, betaMod2, type = "latex", dep.var.labels   = "Monthly HCF at Kapichira", add.lines = list(c("Month fixed effects", "No", "Yes")))

daily2$cf_tedzani = ifelse(daily2$cf_tedzani>=1, 0.99, daily2$cf_tedzani)
daily2$cf_tedzani = ifelse(daily2$cf_kapichira==0, 0.0001, daily2$cf_kapichira)

betaMod1 <- betareg(cf_tedzani ~  deviation_discharge, data = daily2)
summary (betaMod1) 
summary(margins(betaMod1))

betaMod2 <- betareg(cf_tedzani ~  deviation_discharge + factor(month), data = daily2) 
summary (betaMod2) 
summary(margins(betaMod2))

stargazer(betaMod1, betaMod2, type = "latex", dep.var.labels   = "Monthly HCF at Tedzani", add.lines = list(c("Month fixed effects", "No", "Yes")))

#Define a measure for extreme events as droughtflood = 1 if dd > 2*sd(DD), else droughtflood=0
daily2 = daily2 %>% group_by(month) %>%  mutate(droughtflood = ifelse(deviation_discharge < unname(quantile(deviation_discharge, 0.05, na.rm=TRUE)), 1, 0)) %>% ungroup()

#12) Assess the impact of extreme events of hydropower capacity factor
betaMod1 <- betareg(capfactor ~  droughtflood, data = daily2) # train model. Tune 
summary (betaMod1) 
summary(margins(betaMod1))

betaMod2 <- betareg(capfactor ~  droughtflood + as.factor(month), data = daily2) 
summary (betaMod2) 
summary(margins(betaMod2))

stargazer(betaMod1, betaMod2, type = "latex", dep.var.labels   = "Total monthly hydropower capacity factor", add.lines = list(c("Month fixed effects", "No", "Yes")))

#5) import night lights data from densely populated areas (monthly VIIRS between 2014-2018) from Earth Engine Processing
source_python("Earth_engine_NLIGHTS.py")
#import into R and reshape
nl_provinces = read.csv("lightts_malawi.csv") 
nl_provinces = dplyr::select(nl_provinces, matches("01|name"))
nl_provinces = nl_provinces[ , order(names(nl_provinces))]
nl_provinces_long = merged.stack(nl_provinces, var.stubs = "X", sep = "var.stubs")
nl_provinces_long$date = nl_provinces_long$.time_1

#But also reshape wide and merge so as to have individual provinces
p =  dplyr::select(nl_provinces_long, NAME_1, date, X) 
p = tidyr::spread(p, key = NAME_1, value = X)
p$date = as.Date(p$date, format="%Y%m%d")
p$month=month(p$date)
p$year=year(p$date)
p$NTL_total=rowSums(select(p, -date))

library(KODAMA)

##Calculate monthly hydropower capacity factor
daily3 = dplyr::select(daily2, total, month, inoperationcapacity, year, deviation_discharge, droughtflood) 

daily3 = group_by(daily3, year, month) %>% dplyr::summarise(inoperationcapacity = median(inoperationcapacity, na.rm=TRUE), droughtflood=ifelse(mean(droughtflood, na.rm = TRUE)>0.25, 1, 0), total=sum(total, na.rm = TRUE), deviation_discharge=mean(deviation_discharge, na.rm=TRUE))

daily3$capfactor = ((daily3$total/1000)/(daily3$inoperationcapacity*24*30))

merger = merge(daily3, p, by=c("year", "month"))
merger = data.table(merger)
merger = merger[order(year)]

#gb <- ggplot_build(disc_efficiency)

disc_efficiency = ggplot(data=merger)+
  geom_point(aes(x=Liwonde_dis_forecasted, y=capfactor/1000000, colour=month), size=3, alpha=1)+
  stat_smooth(aes(x=Liwonde_dis_forecasted, y=capfactor/1000000), method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE, fullrange = TRUE)+
  xlab("Discharge")+
  xlab(expression(paste("Discharge at Liwonde (", m^3/s, ")", sep = "")))+
  ylab("Monthly total \n hydropower capacity factor")+
  scale_color_continuous(name="Month")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(limits = c(0,350))


ggsave(plot=disc_efficiency, device = "png", filename = "disc_efficiency.png", width = 16, height = 11, scale = 0.4)

#6) Assess the impact of deviation discharge deviation on NTL radiance at the national level in 1-km pixels with > 250 inhabs.
formula<-"NTL_total ~  deviation_discharge"
ols1<-lm(formula,data=merger)
summary(ols1, robust=TRUE) 
summary(margins(ols1))

formula<-"NTL_total ~  capfactor + factor(month)"
ols2<-lm(formula,data=merger)
summary(ols2, robust=TRUE)


stargazer(ols1, ols2, type = "latex", dep.var.labels   = "ln of nighttime light radiance")

## Plot effect

disc_efficiency = ggplot(data=daily2)+
  geom_point(aes(x=MatZaw_dis_forecasted, y=capfactor, colour=month), size=1.5, alpha=0.5)+
  stat_smooth(aes(x=MatZaw_dis_forecasted, y=capfactor), method = "lm", formula = y ~ x + I(x^2), size = 1)+
  xlab("Discharge")+
  xlab(expression(paste("Discharge at Zalewa (", m^3/s, ")", sep = "")))+
  ylab("Daily hydropower capacity factor")+
  theme(legend.position = "none")


bb = ggplot(data=merger)+
  geom_point(aes(x=deviation_discharge_meanmonth, y=NTL_total, colour=month), size=1.5, alpha=0.5)+
  geom_abline(intercept = 0)+
  stat_smooth(aes(x=deviation_discharge_meanmonth, y=NTL_total), method = "lm", formula = y ~ x + I(x^2), size = 1)+
  xlab("Monthly discharge deviation")+
  ylab("Sum of monthly NTL radiance \n in urban areas")+
  scale_color_continuous(name="Month")
  
aabb = cowplot::plot_grid(disc_efficiency, bb, labels = "AUTO", nrow = 1, rel_widths = c(1, 1.3))

ggsave("bb.png", aabb, device = "png", width = 20, height = 8, scale=0.4)


#Assess the impact of extreme events on NTL radiance at the national level in 1-km pixels with > 250 inhabs.
formula<-"log(NTL_total) ~  droughtflood"
ols1<-lm(formula,data=merger)
summary(ols1, robust=TRUE) 

formula<-"log(NTL_total) ~  droughtflood + factor(month)"
ols2<-lm(formula,data=merger)
summary(ols2, robust=TRUE) 

stargazer(ols1, ols2, type = "latex", dep.var.labels   = "ln of nighttime light radiance")

#Assess heterogeneity in impact across provinces
funcol = function(X){ 
  all(X != 0)
}

row_sub = apply(dplyr::select(merger, 9:36), 1, funcol)

prova2 = merger[row_sub,]

fu = function(X){
  paste0("log(`", X, "`+1)", "~", "deviation_discharge_meanmonth + factor(month)")
}

formula<-lapply(colnames(merger[,9:36]), fu)

function_regs = function(X){
  ols1<-lm(X,data=merger)
}

listresults = lapply(formula, function_regs)

n <- 28
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
names=colnames(merger[,9:36])
p = plot_coefs(listresults[[1]], listresults[[2]], listresults[[3]], listresults[[4]], listresults[[5]], listresults[[6]], listresults[[7]], listresults[[8]], listresults[[9]], listresults[[10]], listresults[[11]], listresults[[12]], listresults[[13]], listresults[[14]], listresults[[15]], listresults[[16]], listresults[[17]], listresults[[18]], listresults[[19]], listresults[[20]], listresults[[21]], listresults[[22]], listresults[[23]], listresults[[24]], listresults[[25]], listresults[[26]], listresults[[27]], listresults[[28]], model.names=names, coefs=c("deviation_discharge_meanmonth"), legend.title="Province", point.shape = FALSE, color.class= col_vector, ci_level=0.95)

p1 = p + theme_gray() + theme(legend.position = "none") + xlab("Effect of DD on NTL radiance") + theme(
  axis.text.y = element_blank())


#Assess heterogeneity in impact across provinces for EXTREME EVENTS
fu = function(X){
  paste0("log(`", X, "`+1)", "~", "droughtflood + factor(month)")
}

formula<-lapply(colnames(merger[,9:36]), fu)

function_regs = function(X){
  ols1<-lm(X,data=merger)
}

listresults = lapply(formula, function_regs)

c = vector()
se = vector()
ci_max = vector()
ci_min = vector()

for (i in 1:28){
c[i] = coef(summary(listresults[[i]]))[, "Estimate"][2]
se[i] = coef(summary(listresults[[i]]))[, "Std. Error"][2]
ci_max[i] = c[i] + 1.96*se[i]
ci_min[i] = c[i] - 1.96*se[i]
}

df = data.frame(c, ci_max, ci_min)
df$province = colnames(merger[,9:36])
  
ggplot(df, aes(x=province, y=c))+
  geom_errorbar(aes(ymin=df$ci_min, ymax=df$ci_max))


n <- 28
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
names=colnames(merger[,9:36])
p = plot_coefs(listresults[[1]], listresults[[2]], listresults[[3]], listresults[[4]], listresults[[5]], listresults[[6]], listresults[[7]], listresults[[8]], listresults[[9]], listresults[[10]], listresults[[11]], listresults[[12]], listresults[[13]], listresults[[14]], listresults[[15]], listresults[[16]], listresults[[17]], listresults[[18]], listresults[[19]], listresults[[20]], listresults[[21]], listresults[[22]], listresults[[23]], listresults[[24]], listresults[[25]], listresults[[26]], listresults[[27]], listresults[[28]], model.names=names, coefs=c("droughtflood"), legend.title="Province", point.shape = FALSE, color.class= col_vector, ci_level=0.95)


p2 = p + theme_gray() + theme(legend.position = "left") + xlab("Effect of disch. extremes NTL radiance \n (95% C.I.)") + theme(
  axis.text.y = element_blank())

ggsave(plot=p2, device="png", filename = "plot.png", scale=0.5, width = 13, height = 12)


#7) extract coefficients and create a map with the effects
prova=data.frame(p$data$estimate, p$data$p.value, names)

malawi=read_sf('Other data//gadm36_1.shp')
malawi=subset(malawi, malawi$GID_0=="MWI")
malawi=merge(malawi, prova, by.x="NAME_1", by.y="names")

malawi_grid=read_sf('Shire river//Malawi_grid_lines_FB.shp')

map = ggplot() + geom_sf(data = malawi, aes(fill = p.data.estimate)) + scale_y_continuous(breaks = c(-0.9, 0))+ geom_sf(data = malawi_grid)

plot(malawi_grid)

ggsave(plot=map, device="png", filename = "map_NTL_effect.png", scale=0.4, width = 20, height = 12)


#Import electfication rates from DHS
DHS = read.csv("Other data//Parsing.csv")
elrate_plot_effect=merge(DHS, malawi, by.x="GID_1", by.y="GID_1")

#Plot effect vs. electrification rates
effectelrates = ggplot(data=elrate_plot_effect, aes(x=elaccess/100, y=p.data.estimate))+
  theme_classic()+
  geom_point(size=2.5)+
  geom_smooth(method = "lm", se=FALSE)+
  xlab('Electricity access rate (province)')+
  ylab('Effect of discharge deviations on NTL radiance')+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))

pg = plot_grid(p, effectelrates, labels = "AUTO")

ggsave(plot=pg, device="png", filename = "effectelrates.png", scale=0.45, width = 28, height = 12)

