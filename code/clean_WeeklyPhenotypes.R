#!/usr/bin/env Rscript --vanilla

##############################################################
# Author: Mallory A. Ballinger
# Script first created: 12-Feb-2021
# Script last updated:  07-Apr-2021


# This script cleans the metadata file: data/raw/weekly_metadata_RAW_2021-02-11.csv.
# The generated, cleaned dataset is used for downstream analyses.


##############################################################
# Required packages
##############################################################

rm(list = ls()) # clear R's environment
library(tidyverse)
library(here)

##############################################################
# Import & clean data
##############################################################

WeeklyMetaData <- read_csv(here("data/raw/weekly_metadata_RAW_2021-02-11.csv")) %>%
  rename("Tail_Length_mm" = "TailLength_mm",
         "Body_Weight_g" = "BodyWeight_g") %>%
  filter(Population == "BRAZIL" | Population == "NEW_YORK") %>% # only keep parental populations (remove F1 hybrids)
  filter(Generation == "N11" | Generation == "N12") %>%
  mutate(Sex = fct_recode(Sex, "Female" = "F", "Male" = "M")) %>% # spells out males and females
  mutate(Environment = fct_recode(Environment, "Cold" = "COLD", "Warm" = "RT")) %>%
  mutate(Line = fct_recode(Line, "MANA" = "193x255", "SARA" = "19x13",
                           "MANB" = "222x254", "SARB" = "82x81")) %>% # gives each line the "published"/JAX name
  mutate(Population = fct_recode(Population, "Brazil" = "BRAZIL", "New York" = "NEW_YORK")) %>% # modifies names of populations
  mutate(PopEnv = paste(Population, Environment, sep = "_")) %>%
  mutate(PopEnv = fct_recode(PopEnv, "Brazil - Warm" = "Brazil_Warm", "Brazil - Cold" = "Brazil_Cold",
                             "New York - Warm" = "New York_Warm", "New York - Cold" = "New York_Cold")) %>%
  mutate(Age_weeks = round(Age_weeks, 2))


# Sample size of dataset
FullSampleSize <- WeeklyMetaData %>% summarise(N=n_distinct(Mouse_ID)) %>% pull(N)
# n = 84


write.csv(WeeklyMetaData, file = "results/tables/WeeklyPhenotypeData.csv", row.names = TRUE)
write.csv(WeeklyMetaData, file = "data/processed/WeeklyPhenotypeData.csv", row.names = TRUE)