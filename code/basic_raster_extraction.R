#!/usr/bin/env Rscript --vanilla

##############################################################

# Author: Jesse M. Alston
# Last Updated: 8/29/21
# Modified by Mallory A. Ballinger: 10/26/21

# This script extracts climate data from a raster
# Climate data (mean annual temperature) is used in downstream supplemental
# analyses in Ballinger_AmNat_2021.


##############################################################
# Required packages
##############################################################

rm(list = ls()) # clears R's environment
library(tidyverse)
library(raster)
library(sf)
library(rgdal)
library(here)


##############################################################
# Import data
##############################################################

# Load lat/long data (VertNet Metadata)
mouse_df <- read_csv(here("data/processed/VertNetMetadata_Mus_2021-03-18.csv"))

# Load raster file (one for each month)
mean_temp_1 <- raster("data/raw/wc2.1_30s_tavg_01.tif")
mean_temp_2 <- raster("data/raw/wc2.1_30s_tavg_02.tif")
mean_temp_3 <- raster("data/raw/wc2.1_30s_tavg_03.tif")
mean_temp_4 <- raster("data/raw/wc2.1_30s_tavg_04.tif")
mean_temp_5 <- raster("data/raw/wc2.1_30s_tavg_05.tif")
mean_temp_6 <- raster("data/raw/wc2.1_30s_tavg_06.tif")
mean_temp_7 <- raster("data/raw/wc2.1_30s_tavg_07.tif")
mean_temp_8 <- raster("data/raw/wc2.1_30s_tavg_08.tif")
mean_temp_9 <- raster("data/raw/wc2.1_30s_tavg_09.tif")
mean_temp_10 <- raster("data/raw/wc2.1_30s_tavg_10.tif")
mean_temp_11 <- raster("data/raw/wc2.1_30s_tavg_11.tif")
mean_temp_12 <- raster("data/raw/wc2.1_30s_tavg_12.tif")

# Identify projection system
crs(mean_temp_1)

# Set projection for later use
proj <- "+proj=longlat +datum=WGS84 +no_defs" 

# Crop rasters to extent of data to reduce computational needs (time and RAM) 
# Set these just outside the maximum and minimum longitude and latitude of VertNet Metadata #
e <- as(extent(-171, -19, -60, 75), 'SpatialPolygons')

# Set projection system (should match the raster)
crs(e) <- proj

# Crop the raster using the extent object just created
mean_temp_1c <- crop(mean_temp_1, e)
mean_temp_2c <- crop(mean_temp_2, e)
mean_temp_3c <- crop(mean_temp_3, e)
mean_temp_4c <- crop(mean_temp_4, e)
mean_temp_5c <- crop(mean_temp_5, e)
mean_temp_6c <- crop(mean_temp_6, e)
mean_temp_7c <- crop(mean_temp_7, e)
mean_temp_8c <- crop(mean_temp_8, e)
mean_temp_9c <- crop(mean_temp_9, e)
mean_temp_10c <- crop(mean_temp_10, e)
mean_temp_11c <- crop(mean_temp_11, e)
mean_temp_12c <- crop(mean_temp_12, e)

# Check a raster to make sure it covers the desired area and looks accurate
plot(mean_temp_1c)

# Remove original (larger) raster to save RAM
rm(mean_temp_1, mean_temp_2, mean_temp_3, mean_temp_4, mean_temp_5, mean_temp_6, 
   mean_temp_7, mean_temp_8, mean_temp_9, mean_temp_10, mean_temp_11, mean_temp_12)

# Make data a spatial format
mouse_sp <- mouse_df
coordinates(mouse_sp) <- c("Longitude","Latitude")
proj4string(mouse_sp) <- CRS(proj)

# Extract to a column in VertNet Metadata
mouse_df$mean_temp1 <- raster::extract(mean_temp_1c, spTransform(mouse_sp, CRS(proj)))
mouse_df$mean_temp2 <- raster::extract(mean_temp_2c, spTransform(mouse_sp, CRS(proj)))
mouse_df$mean_temp3 <- raster::extract(mean_temp_3c, spTransform(mouse_sp, CRS(proj)))
mouse_df$mean_temp4 <- raster::extract(mean_temp_4c, spTransform(mouse_sp, CRS(proj)))
mouse_df$mean_temp5 <- raster::extract(mean_temp_5c, spTransform(mouse_sp, CRS(proj)))
mouse_df$mean_temp6 <- raster::extract(mean_temp_6c, spTransform(mouse_sp, CRS(proj)))
mouse_df$mean_temp7 <- raster::extract(mean_temp_7c, spTransform(mouse_sp, CRS(proj)))
mouse_df$mean_temp8 <- raster::extract(mean_temp_8c, spTransform(mouse_sp, CRS(proj)))
mouse_df$mean_temp9 <- raster::extract(mean_temp_9c, spTransform(mouse_sp, CRS(proj)))
mouse_df$mean_temp10 <- raster::extract(mean_temp_10c, spTransform(mouse_sp, CRS(proj)))
mouse_df$mean_temp11 <- raster::extract(mean_temp_11c, spTransform(mouse_sp, CRS(proj)))
mouse_df$mean_temp12 <- raster::extract(mean_temp_12c, spTransform(mouse_sp, CRS(proj)))

# Calculate average over all months (change columns to match your data.frame)
mouse_df$mean_temp <- rowMeans(mouse_df[,22:33])

# Delete the unneeded monthly rows in final data.frame afterward

write.csv(mouse_df, file = "data/processed/VertNetMetadata_Mus_2021-03-18_mean_temp.csv", row.names = TRUE)
