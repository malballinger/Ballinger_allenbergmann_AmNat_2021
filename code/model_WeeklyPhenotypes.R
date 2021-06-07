#!/usr/bin/env Rscript --vanilla

################################################################################

# Author: Mallory A. Ballinger

# Currently, do not have any statistical analyses to implement with this dataset.


################################################################################
# Required packages
################################################################################

rm(list = ls()) # clear R's environment
library(tidyverse)
library(here)
library(performance)
library(see)
library(car)
library(nlme)
library(Hmisc)
library(dotwhisker)

set.seed(19910118)

#set global contrasts
options(contrasts = c("contr.sum", "contr.poly")) # for Type III SSS


################################################################################
# Import data               
################################################################################

WeeklyPhenotypeData <- read_csv(here("data/processed/WeeklyPhenotypeData.csv")) %>%
  select(-DOB, -DateMeasured) %>% select(-1)
