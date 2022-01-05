#!/usr/bin/env Rscript --vanilla

################################################################################

# Author: Mallory A. Ballinger

# This script models body mass and relative extremity length from colony house mice
# of Brazil and New York populations (i.e. common garden experiment #1).
# Generations include founders (N0) through generation four (N4).
# Data were cleaned in ./clean_Generations_N0-N4.R
# This script generates statistical analyses, and results of analyses are
# specified in Tables S2 in Ballinger_AmNat_2021.


################################################################################
# Required packages
################################################################################

rm(list = ls()) # clear R's environment
library(tidyverse)
library(here)
library(performance)
library(see)
library(lmerTest)
library(lme4)
library(emmeans)
library(car)
library(nlme)
library(Hmisc)
library(dotwhisker)
library(report)
library(patchwork)
library(effectsize)

set.seed(19910118)

#set global contrasts
options(contrasts = c("contr.sum", "contr.poly")) # for Type III SSS


################################################################################
# Import data               
################################################################################

GenerationMetaData <- read_csv(here("data/processed/GenerationColonyData_N0-N4.csv")) %>%
  select(-1, -GUID, -Mother_Line, -Father_Line, -Parental_ID, -Sample_ID, -Total_Length_mm,
         -Hindfoot_Length_mm) %>%
  filter(is.na(Notes) | Notes != "pregnant") %>% select(-Notes) %>%
  mutate(Sex = fct_relevel(Sex, "Male", "Female")) %>% # put males before females
  mutate(Population = fct_relevel(Population, "New York", "Brazil")) %>% # put evolved cold pop first
  mutate(Generation = fct_relevel(Generation, "N0", "N1", "N2", "N3", "N4"))

# get sample size of each column
#colSums(!is.na(GenerationMetaData))


################################################################################
# Data and Model testing
# *Bergmann's rule*
################################################################################

# Check for and remove *extreme* outliers

#GenerationMetaData %>% ggplot(aes(x=Body_Weight_g)) + geom_histogram(binwidth = 1)

# BWOutliers <- GenerationMetaData %>%
#    summarise(GenerationMetaData, meanBW = mean(Body_Weight_g, na.rm = TRUE),
#             sdBW = sd(Body_Weight_g, na.rm = TRUE)) %>%
#    filter(Body_Weight_g < (meanBW - 3*sdBW) |
#           Body_Weight_g > (meanBW + 3*sdBW))

# no clear extreme outliers *below* 3 stdev from mean to remove


# Full Model
mod.full.BW <- lmer(Body_Weight_g ~ Sex * Population * Generation + (1|Line_ID),
                  data = GenerationMetaData)
#check_model(mod.full.BW) # looks good!

# Stats
#summary(mod.full.BW)
#report(mod.full.BW)
# One-way ANOVA P test
Anova_mod.full.BW <- car::Anova(mod.full.BW, type = "III")

# Posthoc Tukey's test
## Sex:Generation
# post_BW_SG <- emmeans::emmeans(mod.full.BW, ~ Sex * Generation, adjust = "sidak")
# pwpp(post_BW_SG)
# pairs(post_BW_SG)

## Population:Generation
# post_BW_PG <- emmeans::emmeans(mod.full.BW, ~ Population * Generation, adjust = "sidak")
# pwpp(post_BW_PG)
# pairs(post_BW_PG)

# New York is larger than Brazil across all generations

effect_size_generations_BW <- omega_squared(mod.full.BW)
#eta_squared(mod.full.BW)


################################################################################
# Data and Model testing
# *Allen's rule* - tail length
################################################################################

# Check for and remove *extreme* outliers (3 stdev from the mean)
#GenerationMetaData %>% ggplot(aes(x=Tail_Length_mm)) + geom_histogram(binwidth = 1)

# TLOutliers <- GenerationMetaData %>%
#   summarise(GenerationMetaData, meanTL = mean(Tail_Length_mm, na.rm = TRUE),
#             sdTL = sd(Tail_Length_mm, na.rm = TRUE)) %>%
#   filter(Tail_Length_mm < (meanTL - 3*sdTL) |
#          Tail_Length_mm > (meanTL + 3*sdTL))

# n = 2 outliers below 50 mm (below 3 stdev from the mean)

Generation_filtered <- GenerationMetaData %>%
  mutate(Tail_Length_mm = ifelse(Tail_Length_mm < 50, NA, Tail_Length_mm)) %>%
  mutate(RelativeTail = (Tail_Length_mm) / (Body_Weight_g))

#Generation_filtered %>% ggplot(aes(x=Tail_Length_mm)) + geom_histogram(binwidth = 1)

# Full Model
mod.full.TL <- lmer(RelativeTail ~ Sex * Population * Generation + (1|Line_ID),
                  data = Generation_filtered)
#check_model(mod.full.TL) # looks great!

# Stats
#summary(mod.full.TL.2)
#report(mod.full.TL.2)
# One-way ANOVA P test
Anova_mod.full.TL <- car::Anova(mod.full.TL, type = "III")

## Population:Generation
# posthoc_TL_PG <- emmeans(mod.full.TL.2, ~ Population * Generation, adjust = "sidak")
# pairs(posthoc_TL_PG)
# pwpp(posthoc_TL_PG)

# Brazil has longer tails than New York across the first two generations

effect_size_generations_TL <- omega_squared(mod.full.TL)
#eta_squared(mod.full.TL.2)


################################################################################
# Data and Model testing
# *Allen's rule* - ear length
################################################################################

# Check for and remove *extreme* outliers
#GenerationMetaData %>% ggplot(aes(x=Ear_Length_mm)) + geom_histogram(binwidth = 1)

# ELOutliers <- GenerationMetaData %>%
#   summarise(GenerationMetaData, meanEL = mean(Ear_Length_mm, na.rm = TRUE),
#             sdEL = sd(Ear_Length_mm, na.rm = TRUE)) %>%
#   filter(Ear_Length_mm < (meanEL - 3*sdEL) |
#          Ear_Length_mm > (meanEL + 3*sdEL))

# n = 1 outliers below 8mm (*below* 3 stdev from mean)

Generation_filtered <- Generation_filtered %>%
  mutate(Ear_Length_mm = ifelse(Ear_Length_mm < 8, NA, Ear_Length_mm))  %>%
  mutate(RelativeEar = (Ear_Length_mm) / (Body_Weight_g))

#Generation_filtered %>% ggplot(aes(x=Ear_Length_mm)) + geom_histogram(binwidth = 1)


# Full Model
mod.full.EL <- lmer(RelativeEar ~ Sex * Population * Generation + (1|Line_ID),
                    data = Generation_filtered)
#check_model(mod.full.EL) # looks great!

# Stats
#summary(mod.full.EL)
Anova_mod.full.EL <- car::Anova(mod.full.EL, type = "III")

# Posthoc Tukey's test:
## Sex:Generation
# posthoc_EL_SG <- emmeans(mod.full.EL, ~ Sex * Generation)
# pairs(posthoc_EL_SG)
# pwpp(posthoc_EL_SG)

## Population:Generation
# posthoc_EL_PG <- emmeans(mod.full.EL, ~ Population * Generation, adjust = "sidak")
# pairs(posthoc_EL_PG)
# pwpp(posthoc_EL_PG)

# Brazil has longer ears than New York across all generations

effect_size_generations_EL <- omega_squared(mod.full.EL)
#eta_squared(mod.full.EL)
