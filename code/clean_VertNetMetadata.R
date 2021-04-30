#!/usr/bin/env Rscript --vanilla

##############################################################

# Author: Mallory A. Ballinger

# This script cleans the metadata file: data/processed/VertNet_Mus_processed.xlsx
# The generated, cleaned dataset is used for downstream analyses in Ballinger_AmNat_2021.


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

VertNetMetaData <- read_xlsx(here("data/processed/VertNet_Mus_processed.xlsx"),
                             col_types = NULL) %>%
  select(references, dynamicproperties, totallength_mm, taillength_mm, hindfootlength_mm,
         earlength_mm, bodyweight_g, bodylength_mm, sex, lifestage, reproductivecondition,
         year, month, continent, country, stateprovince, decimallatitude, decimallongitude,
         scientificname) %>%
  rename("Reference" = "references",
         "Dynamic_Properties" = "dynamicproperties",
         "Total_Length_mm" = "totallength_mm",
         "Tail_Length_mm" = "taillength_mm",
         "Hindfoot_Length_mm" = "hindfootlength_mm",
         "Ear_Length_mm" = "earlength_mm",
         "Body_Weight_g" = "bodyweight_g",
         "Body_Length_mm" = "bodylength_mm",
         "Sex" = "sex",
         "Lifestage" = "lifestage",
         "Reproductive_Status" = "reproductivecondition",
         "Month_Collected" = "month",
         "Year_Collected" = "year",
         "Continent" = "continent",
         "Country" = "country",
         "State_Prov" = "stateprovince",
         "Latitude" = "decimallatitude",
         "Longitude" = "decimallongitude",
         "Species" = "scientificname") %>%
  filter(Continent == "North America" | Continent == "South America") %>%
  filter(is.na(Lifestage) | Lifestage == "adult") %>% # only keep adults and NAs
  filter(is.na(Reproductive_Status) | Reproductive_Status != "preg") %>% # everything but pregnant mice (including NAs)
  filter(Sex == "female" | Sex == "male") %>% # only males and females
  mutate(Absolute_Latitude = abs(Latitude))


# Sample size of dataset
FullSampleSize <- VertNetMetaData %>% summarise(N=n_distinct(Reference)) %>% pull(N)
# n = 3,018


write.csv(VertNetMetaData, file = "results/tables/VertNetMetadata_Mus.csv", row.names = TRUE)
write.csv(VertNetMetaData, file = "data/processed/VertNetMetadata_Mus_2021-03-18.csv", row.names = TRUE)