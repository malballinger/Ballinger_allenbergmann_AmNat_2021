#!/usr/bin/env Rscript --vanilla

##############################################################
# Author: Mallory A. Ballinger
# Script first created: 31-Mar-2021
# Script last updated:  07-Apr-2021


# This script cleans the metadata file: data/raw/colony_metadata_RAW.xlsx
# The generated, cleaned dataset is used for downstream analyses.


##############################################################
# Required packages
##############################################################

rm(list = ls()) # clear R's environment
library(tidyverse)
library(here)
library(readxl)

##############################################################
# Import & clean data
##############################################################

GenerationMetaData <- read_xlsx(here("data/raw/colony_metadata_RAW.xlsx"),
                             col_types = NULL) %>%
  rename("Total_Length_mm" = "TotalLength_mm",
         "Tail_Length_mm" = "TailLength_mm",
         "Hindfoot_Length_mm" = "HindfootLength_mm",
         "Ear_Length_mm" = "EarLength_mm",
         "Body_Weight_g" = "BodyMass_g",
         "Body_Length_mm" = "BodyLength_mm") %>%
  mutate(Sex = fct_recode(Sex, "Female" = "F", "Male" = "M")) %>%
  mutate(Population = fct_recode(Population, "Brazil" = "BRAZIL", "New York" = "NEW_YORK"))


# Sample size of dataset
FullSampleSize <- nrow(GenerationMetaData)
# n = 451


write.csv(GenerationMetaData, file = "results/tables/GenerationColonyData.csv", row.names = TRUE)
write.csv(GenerationMetaData, file = "data/processed/GenerationColonyData.csv", row.names = TRUE)