#!/usr/bin/env Rscript --vanilla

# name: weekly_phenotypes_graphed.R
#
# Plot evolved and plastic differences in weekly body weights and tail lengths
#
# input:
#       - data/raw/WeeklyMetaData
# output:
#       - XXXX

# Clear R and load libraries
rm(list = ls())
library(tidyverse)

# Read in data
WeeklyMetaData <- read_csv(here(data/raw/WeeklyMetaData)) %>%
  select(-DOB, -DateMeasured)

# Clean data of any weird spaces, etc.
WeeklyMetaData[] <- lapply(WeeklyMetaData, function(x) if(is.factor(x)) factor(x) else x)

# Create 'PopByEnv' column
WeeklyData <- WeeklyMetaData %>%
  mutate(PopByEnv = paste(Population, Environment))

#### Explore the data ####


#### Plot all lines by sex ####
# Males only
MaleData <- WeeklyData %>%
  filter(Sex =="M")