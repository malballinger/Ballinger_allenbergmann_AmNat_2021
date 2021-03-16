#!/usr/bin/env Rscript --vanilla

##############################################################
# Author: Mallory A. Ballinger
# Script first created: 15-Feb-2021
# Script last updated:  15-Mar-2021


# This script cleans the metadata file:
# "EnvAdapProj_Nachman_Arctos_transects_2021-03-15.csv"
# The cleaned dataset will be used for downstream analyses.


##############################################################
# Required packages
##############################################################

rm(list = ls()) # clear R's environment
library(tidyverse)
library(here)

##############################################################
# Import & clean data
##############################################################

NachmanTransectsMetadata <- read_csv(here("data/raw/EnvAdapProj_Nachman_Arctos_transects_2021-03-15.csv"),
                                     col_names = TRUE,
                                     cols(
                                       USE_LICENSE_URL = col_character(),
                                       COLLECTOR_ID = col_character(),
                                       GUID = col_character(),
                                       SCIENTIFIC_NAME = col_character(),
                                       GENUS = col_character(),
                                       SPECIES = col_character(),
                                       COLLECTORS = col_character(),
                                       CONTINENT_OCEAN = col_character(),
                                       COUNTRY = col_character(),
                                       STATE_PROV = col_character(),
                                       COUNTY = col_character(),
                                       MIN_ELEV_IN_M = col_double(),
                                       MAX_ELEV_IN_M = col_double(),
                                       MINIMUM_ELEVATION = col_double(),
                                       MAXIMUM_ELEVATION = col_double(),
                                       ELEV_IN_M = col_character(),
                                       DEPTH_IN_M = col_logical(),
                                       SPEC_LOCALITY = col_character(),
                                       LOCALITY_REMARKS = col_character(),
                                       VERBATIM_LOCALITY = col_character(),
                                       VERBATIM_DATE = col_character(),
                                       DAY_COLLECTED = col_double(),
                                       MONTH_COLLECTED = col_double(),
                                       YEAR_COLLECTED = col_double(),
                                       VERBATIM_COORDINATES = col_character(),
                                       ORIG_LAT_LONG_UNITS = col_character(),
                                       DEC_LAT = col_double(),
                                       DEC_LONG = col_double(),
                                       COORDINATEUNCERTAINTYINMETERS = col_double(),
                                       DATUM = col_character(),
                                       FORMATTED_SCIENTIFIC_NAME = col_character(),
                                       COLL_EVENT_REMARKS = col_character(),
                                       ASSOCIATED_SPECIES = col_logical(),
                                       AGE = col_logical(),
                                       AGE_CLASS = col_character(),
                                       BODY_CONDITION = col_logical(),
                                       BODY_WIDTH = col_logical(),
                                       CALCAR_LENGTH = col_logical(),
                                       DESCRIPTION = col_logical(),
                                       DIMENSIONS = col_logical(),
                                       EAR_FROM_NOTCH = col_character(),
                                       EAR_LENGTH_MM = col_double(),
                                       EMBRYO_WEIGHT = col_logical(),
                                       HEAD_LENGTH = col_logical(),
                                       HEAD_WIDTH = col_logical(),
                                       HEIGHT = col_logical(),
                                       HIND_FOOT_WITH_CLAW = col_character(),
                                       HIND_FOOT_LENGTH_MM = col_double(),
                                       HIND_LIMB_LENGTH = col_logical(),
                                       LEFT_GONAD_LENGTH = col_logical(),
                                       LEFT_GONAD_WIDTH = col_logical(),
                                       MAXIMUM_STANDARD_LENGTH = col_logical(),
                                       MAXIMUM_TOTAL_LENGTH = col_logical(),
                                       MINIMUM_STANDARD_LENGTH = col_logical(),
                                       MINIMUM_TOTAL_LENGTH = col_logical(),
                                       REPRODUCTIVE_DATA = col_character(),
                                       REPRODUCTIVE_STATUS = col_character(),
                                       RIGHT_GONAD_LENGTH = col_logical(),
                                       RIGHT_GONAD_WIDTH = col_logical(),
                                       SEX = col_character(),
                                       STANDARD_LENGTH = col_logical(),
                                       TAIL_BASE_WIDTH = col_logical(),
                                       TAIL_CONDITION = col_logical(),
                                       TAIL_LENGTH = col_character(),
                                       TAIL_LENGTH_MM = col_double(),
                                       TOTAL_LENGTH = col_character(),
                                       TOTAL_LENGTH_MM = col_double(),
                                       VERBATIM_COLLECTOR = col_logical(),
                                       WEIGHT = col_character(),
                                       BODY_WEIGHT_G = col_double(),
                                       BODY_LENGTH_MM = col_double(),
                                       NOTES = col_character() )) %>%
  select(COLLECTOR_ID, GUID, SPECIES, CONTINENT_OCEAN, COUNTRY, STATE_PROV,
         MIN_ELEV_IN_M, MONTH_COLLECTED, YEAR_COLLECTED, DEC_LAT, DEC_LONG,
         AGE_CLASS, EAR_LENGTH_MM, HIND_FOOT_LENGTH_MM, REPRODUCTIVE_DATA,
         REPRODUCTIVE_STATUS, SEX, TAIL_LENGTH_MM, TOTAL_LENGTH_MM, BODY_WEIGHT_G,
         BODY_LENGTH_MM) %>%
  rename("Collector_ID" = "COLLECTOR_ID",
         "Species" = "SPECIES",
         "Continent" = "CONTINENT_OCEAN",
         "Country" = "COUNTRY",
         "State_Prov" = "STATE_PROV",
         "Minimum_elevation_m" = "MIN_ELEV_IN_M",
         "Month_Collected" = "MONTH_COLLECTED",
         "Year_Collected" = "YEAR_COLLECTED",
         "Latitude" = "DEC_LAT",
         "Longitude" = "DEC_LONG",
         "Age" = "AGE_CLASS",
         "Ear_Length_mm" = "EAR_LENGTH_MM",
         "Hindfoot_Length_mm" = "HIND_FOOT_LENGTH_MM",
         "Reproductive_Data" = "REPRODUCTIVE_DATA",
         "Reproductive_Status" = "REPRODUCTIVE_STATUS",
         "Sex" = "SEX",
         "Tail_Length_mm" = "TAIL_LENGTH_MM",
         "Total_Length_mm" = "TOTAL_LENGTH_MM",
         "Body_Length_mm" = "BODY_LENGTH_MM",
         "Body_Weight_g" = "BODY_WEIGHT_G") %>%
  filter(is.na(Age) | Age == "adult") %>% # only adults and NAs
  filter(is.na(Minimum_elevation_m) | Minimum_elevation_m < 1400) %>% # everything below 1400m elevation (>1500m is considered HA)
  filter(is.na(Reproductive_Status) | Reproductive_Status != "pregnant") %>% # everything but pregnant mice (including NAs)
  mutate(Absolute_Latitude = abs(Latitude)) %>%
  select(-Reproductive_Status)

# Sample size of dataset
FullSampleSize <- NachmanTransectsMetadata %>% summarize(N=n_distinct(GUID)) %>% pull(N)
# n = 214

write.csv(NachmanTransectsMetadata, file = "results/tables/EnvAdapProj_Nachman_Arctos_transects.csv", row.names = TRUE)
write.csv(NachmanTransectsMetadata, file = "data/processed/EnvAdapProj_Nachman_Arctos_transects_2021-03-15.csv", row.names = TRUE)