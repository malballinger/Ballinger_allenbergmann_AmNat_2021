#!/usr/bin/env Rscript --vanilla

##############################################################

# Author: Mallory A. Ballinger

# This script cleans the metadata file: data/raw/colony_metadata_RAW_v2.xlsx
# The generated, cleaned data set is used for downstream analyses in Ballinger_AmNat_2021.


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

h2MetaData <- read_xlsx(here("data/raw/N2vsN3_h2.xlsx"),
                        col_types = NULL) %>%
  rename("Total_Length_mm" = "TotalLength_mm",
         "Tail_Length_mm" = "TailLength_mm",
         "Hindfoot_Length_mm" = "HindfootLength_mm",
         "Ear_Length_mm" = "EarLength_mm",
         "Body_Weight_g" = "BodyMass_g",
         "Body_Length_mm" = "BodyLength_mm") %>%
  mutate(Sex = fct_recode(Sex, "Female" = "F", "Male" = "M")) %>%
  mutate(Population = fct_recode(Population, "Brazil" = "BRAZIL", "New York" = "NEW_YORK"))


# Sample size of data set
FullSampleSize <- nrow(h2MetaData)
# n = 126


# Sample size of New York
NewYork_lines <- h2MetaData %>%
  filter(Population == "New York") %>%
  select(Line_ID)
n_distinct(NewYork_lines) # 10 lines

NewYork_families <- h2MetaData %>%
  filter(Population == "New York") %>%
  select(Midparent_ID)
n_distinct(NewYork_families) # 16


# Sample size of Brazil
Brazil_lines <- h2MetaData %>%
  filter(Population == "Brazil") %>%
  select(Line_ID)
n_distinct(Brazil_lines) # 13 lines

Brazil_families <- h2MetaData %>%
  filter(Population == "Brazil") %>%
  select(Midparent_ID)
n_distinct(Brazil_families) # 16


write.csv(h2MetaData, file = "results/tables/N2N3_h2_data.csv", row.names = TRUE)
write.csv(h2MetaData, file = "data/processed/N2N3_h2_data.csv", row.names = TRUE)