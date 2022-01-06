#!/usr/bin/env Rscript --vanilla

################################################################################

# Author: Mallory A. Ballinger

# This script models body mass and relative extremity lengths from house mice of common garden
# experiment #2. Data were cleaned using ./clean_RXNs.R.
# This script generates statistical analyses for Ballinger_AmNat_2021, reported in Table 1.


################################################################################
# Required packages
################################################################################

rm(list = ls()) # clears R's environment
library(tidyverse)
library(here)
library(performance)
library(see)
library(lmerTest)
library(lme4)
library(emmeans)
library(car)
library(nlme)
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

PostDissectionMetaData <- read_csv(here("data/processed/PostDissectionMetaData.csv")) %>%
  select(-1) %>% # removes first column
  mutate(Sex = fct_relevel(Sex, "Male", "Female")) %>% # put males before females
  mutate(Population = fct_relevel(Population, "New York", "Brazil")) %>% # put evolved cold pop first
  mutate(Line = fct_relevel(Line, "SARA", "SARB", "MANA", "MANB")) %>% # put evolved cold lines first
  mutate(Environment = fct_relevel(Environment, "Warm", "Cold" )) %>% # put warm before cold (evolved before plasticity)
  mutate(RelativeTail = (Final_Tail_Length_mm) / (Body_Weight_g))


# get sample size of each column
#colSums(!is.na(PostDissectionMetaData))


# Sex-specific datasets
MaleData <- PostDissectionMetaData %>%
  filter(Sex == "Male")

FemaleData <- PostDissectionMetaData %>%
  filter(Sex == "Female")


################################################################################
# Data and Model testing
# *Bergmann's rule*
################################################################################

#PostDissectionMetaData %>% ggplot(aes(x=Body_Weight_g)) + geom_histogram(binwidth = 1)

# BWOutliers <- PostDissectionMetaData %>%
#    summarise(PostDissectionMetaData, meanBW = mean(Body_Weight_g, na.rm = TRUE),
#             sdBW = sd(Body_Weight_g, na.rm = TRUE)) %>%
#    filter(Body_Weight_g < (meanBW - 3*sdBW) |
#           Body_Weight_g > (meanBW + 3*sdBW))

# no clear extreme outliers below 3 stdev from mean to remove

# Full model - all sexes
mod.full.BW <- lmer(Body_Weight_g ~ Sex * Population * Environment + (1|Line),
                    data = PostDissectionMetaData)
#check_model(mod.full.BW) looks great

# Anova P test
#anova(mod.full.BW) # Satterthwaite's method
Anova_mod.full.BW <- car::Anova(mod.full.BW, type = "III")

effect_size_RXN_BW <- omega_squared(mod.full.BW)


#PostDissectionMetaData %>% ggplot(aes(x=BMI_kg_m2)) + geom_histogram(binwidth = 1)

# BMIOutliers <- PostDissectionMetaData %>%
#    summarise(PostDissectionMetaData, meanBW = mean(BMI_kg_m2, na.rm = TRUE),
#             sdBW = sd(BMI_kg_m2, na.rm = TRUE)) %>%
#    filter(BMI_kg_m2 < (meanBW - 3*sdBW) |
#             BMI_kg_m2 > (meanBW + 3*sdBW))

# no outliers that are 3 stdev below mean

# Full model - BMI
mod.full.BMI <- lmer(BMI_kg_m2 ~ Sex * Population * Environment + (1|Line),
                     data = PostDissectionMetaData)
#check_model(mod.full.BMI)
#shapiro.test(resid(mod.full.BMI)) # normally distributed


# Stats - BMI
summary(mod.full.BMI)
report(mod.full.BMI)
# Anova P test
car::Anova(mod.full.BMI, type = "III")


################################################################################
# Data and Model testing
# *Allen's rule* - tail length
################################################################################

# Refer to exploratory/Modeling_RXN_2021-02-19.Rmd' for initial model testing and exploration.
 
# no outliers below 3 stdev from mean


# Full model - all sexes
mod.full.TL <- lmer(RelativeTail ~ Sex * Population * Environment + (1|Line),
                    data = PostDissectionMetaData, REML = T)
#check_model(mod.full.TL) # looks great


# Anova P test
Anova_mod.full.TL <- car::Anova(mod.full.TL, type = "III")

effect_size_RXN_TL <- omega_squared(mod.full.TL)


################################################################################
# Data and Model testing
# *Allen's rule* - ear length
################################################################################

#PostDissectionMetaData %>% ggplot(aes(x=Ear_Length_mm)) + geom_histogram(binwidth = 1)

# ELOutliers <- PostDissectionMetaData %>%
#    summarise(PostDissectionMetaData, meanEL = mean(Ear_Length_mm, na.rm = TRUE),
#             sdEL = sd(Ear_Length_mm, na.rm = TRUE)) %>%
#    filter(Ear_Length_mm < (meanEL - 3*sdEL) |
#          Ear_Length_mm > (meanEL + 3*sdEL))

# n = 1 outliers above 17mm (above 3 stdev from mean)

PostDissection_filtered <- PostDissectionMetaData %>%
  mutate(Ear_Length_mm = ifelse(Ear_Length_mm > 17, NA, Ear_Length_mm)) %>%
  mutate(RelativeEar = (Ear_Length_mm) / (Body_Weight_g))

# dataset for ear length > n = 78


# Full model - all sexes
mod.full.EL <- lmer(RelativeEar ~ Sex * Population * Environment + (1|Line),
                    data = PostDissection_filtered)
#check_model(mod.full.EL) # looks good

# Anova NP test
Anova_mod.full.EL <- car::Anova(mod.full.EL, type = "III")

effect_size_RXN_EL <- omega_squared(mod.full.EL)

