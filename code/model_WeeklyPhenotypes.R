#!/usr/bin/env Rscript --vanilla

################################################################################
# Author: Mallory A. Ballinger
# Script first created: 03-Apr-2021
# Script last updated:  09-Apr-2021


# This script models body mass and extremity length from house mice of common garden
# experiment #2. Data were cleaned using the script ./clean_WeeklyPhenotypes.R.
# This script generates statistical analyses for Ballinger_et_al_2021_AmNat.


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

################################################################################
# Import data               
################################################################################

WeeklyPhenotypeData <- read_csv(here("data/processed/WeeklyPhenotypeData.csv")) %>%
  select(-DOB, -DateMeasured) %>% select(-1)

