#!/usr/bin/env Rscript --vanilla

##############################################################
# Author: Mallory A. Ballinger
# Script first created: 12-Feb-2021
# Script last updated:  07-Apr-2021


# This script cleans the metadata file: data/raw/post_dissection_metadata_RAW_2021-02-18.csv.
# The generated, cleaned dataset is used for downstream analyses.


##############################################################
# Required packages
##############################################################

rm(list = ls()) # clears R's environment
library(tidyverse)
library(here)

##############################################################
# Import & clean data
##############################################################

PostDissectionMetaData <- read_csv(here("data/raw/post_dissection_metadata_RAW_2021-02-18.csv")) %>%
  select(-WeaningBodyWeight_g, -WeaningTailLength_mm, -StartExpBodyWeight_g,
         -StartExpTailLength_mm, -BAT_mass_g) %>% select(-last_col()) %>%
  rename("Total_Length_mm" = "TotalLength_mm",
         "Tail_Length_mm" = "TailLength_mm",
         "Hindfoot_Length_mm" = "HindFootLength_mm",
         "Ear_Length_mm" = "EarLength_mm",
         "Body_Weight_g" = "BodyWeight_g",
         "Final_Tail_Length_mm" = "FinalTailLength_mm",
         "Body_Length_mm" = "BodyLength_mm") %>%
  filter(Population == "BRAZIL" | Population == "NEW_YORK") %>% # only keep parental populations (remove F1 hybrids)
  mutate(Population = fct_recode(Population, "Brazil" = "BRAZIL", "New York" = "NEW_YORK")) %>% # modifies names of populations
  filter(Generation == "N11" | Generation == "N12") %>%
  mutate(Sex = fct_recode(Sex, "Female" = "F", "Male" = "M")) %>% # spells out males and females
  mutate(Environment = fct_recode(Environment, "Cold" = "COLD", "Warm" = "RT")) %>%
  mutate(Line = fct_recode(Line, "MANA" = "193x255", "SARA" = "19x13",
                           "MANB" = "222x254", "SARB" = "82x81")) %>% # gives each line the JAX name
  mutate(Age_weeks = round(Age_weeks, 2))

# Sample size of dataset
FullSampleSize <- PostDissectionMetaData %>% summarise(N=n_distinct(Mouse_ID)) %>% pull(N)
# n = 80


write.csv(PostDissectionMetaData, file = "results/tables/PostDissectionMetaData.csv", row.names = TRUE)
write.csv(PostDissectionMetaData, file = "data/processed/PostDissectionMetaData.csv", row.names = TRUE)