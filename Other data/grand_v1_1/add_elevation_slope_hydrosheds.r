


#source("P:/ene.general/Water/global_basin_modeling/hydrosheds/elevation_change_by_flow_direction.r")

# Clear workspace and close graphics windows
rm(list=ls())
graphics.off()

# Increase memory size
memory.limit(size = 1e6)

# Install libraries
library(raster)
library(rgdal)
library(maptools)
library(rgeos)

# Import Grand v1_1 database
grand_v1_1.spdf = readOGR('P:/ene.general/Water/global_basin_modeling/grand_v1_1','GRanD_dams_v1_1')

# Keep relevant data for manipulation
grand_v1_1_red.spdf = grand_v1_1.spdf[-1*which( !(grand_v1_1.spdf@data$AREA_POLY > 0 & grand_v1_1.spdf@data$DAM_HGT_M > 0 & grand_v1_1.spdf@data$CAP_MCM > 0 ) ),c('DAM_HGT_M','AREA_POLY','CAP_MCM','LAT_DD','LONG_DD')]

# Define continental tiles from hydroBASINS
cnt_list = c('af', 'ar', 'as', 'au', 'eu', 'gr', 'na', 'sa', 'si')

