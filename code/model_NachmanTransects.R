#!/usr/bin/env Rscript --vanilla

################################################################################
# Author: Mallory A. Ballinger
# Script first created: 23-Feb-2021
# Script last updated:  29-Mar-2021


# This script models body mass and extremity length from wild-caught house mice,
# collected across North and South America. Data are from Arctos.org, and were
# cleaned using the script ./clean_NachmanTransects.R.
# This script generates statistical analyses and Tables xxxx in Ballinger_et_al_2021_AmNat.


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

################################################################################
# Import data               
################################################################################

NachmanTransectsMetadata <- read_csv(here("data/processed/EnvAdapProj_Nachman_Arctos_transects_2021-03-15.csv")) %>%
  mutate(Ear_Length_mm = as.numeric(Ear_Length_mm),
         Hindfoot_Length_mm = as.numeric(Hindfoot_Length_mm),
         Tail_Length_mm = as.numeric(Tail_Length_mm),
         Body_Length_mm = as.numeric(Body_Length_mm),
         Body_Weight_g = as.numeric(Body_Weight_g))
  
# get sample size of each column
colSums(!is.na(NachmanTransectsMetadata))

################################################################################
# Data and Model testing
# *Bergmann's rule*
################################################################################

# Check for and remove *extreme* outliers

NachmanTransectsMetadata %>% ggplot(aes(x=Body_Weight_g)) + geom_histogram(binwidth = 1)
# no clear extreme outliers for body weight

# get sample size for Body Weight
length(which(!is.na(NachmanTransectsMetadata$Body_Weight_g)))


# Check if data are normally distributed

lmBW <- lm(Body_Weight_g ~ Absolute_Latitude, data = NachmanTransectsMetadata)
check_model(lmBW)
shapiro.test(resid(lmBW)) # normally distributed





################################################################################
# Data and Model testing
# *Allen's rule* - tail length
################################################################################

# Check for and remove *extreme* outliers

NachmanTransectsMetadata %>% ggplot(aes(x=Tail_Length_mm)) + geom_histogram(binwidth = 1)
# from eyeball test, any tail shorter than 25mm are extreme outliers

NachmanTransects_filtered <- NachmanTransectsMetadata %>%
  mutate(Tail_Length_mm = ifelse(Tail_Length_mm < 25, NA, Tail_Length_mm))

NachmanTransects_filtered %>% ggplot(aes(x = Tail_Length_mm)) + geom_histogram(binwidth = 1)


# Determine if body weight or body length gives a better model for residuals

# First, check for and remove *extreme* outliers for body length
NachmanTransects_filtered %>% ggplot(aes(x=Body_Length_mm)) + geom_histogram(binwidth = 1)
# no clear extreme outliers for body weight


# Make models

lmTLBW <- lm(Tail_Length_mm ~ Body_Weight_g + Absolute_Latitude, data = NachmanTransects_filtered)
check_model(lmTLBW)
shapiro.test(resid(lmTLBW)) # not normally distributed

ggplot(data = NachmanTransects_filtered, aes(x = Body_Weight_g, y = Tail_Length_mm)) +
  geom_point() +
  geom_smooth(method = "lm")

# sample size of lmTLBW resids
length(which(!is.na(resid(lmTLBW))))



lmTLBL <- lm(Tail_Length_mm ~ Body_Length_mm + Absolute_Latitude, data = NachmanTransects_filtered)
check_model(lmTLBL)
shapiro.test(resid(lmTLBL)) # not normally distributed

ggplot(data = NachmanTransects_filtered, aes(x = Body_Length_mm, y = Tail_Length_mm)) +
  geom_point() +
  geom_smooth(method = "lm")

# sample size of lmTLBL resids
length(which(!is.na(resid(lmTLBL))))


compare_performance(lmTLBW, lmTLBL, rank = TRUE)

# body weight gives better model


# save TL x BW residuals
residsTLBW <- lm(Tail_Length_mm ~ Body_Weight_g, data = NachmanTransects_filtered,
                 na.action = na.exclude)
NachmanTransects_filtered$Resids_TLBW <- resid(residsTLBW)





################################################################################
# Data and Model testing
# *Allen's rule* - ear length
################################################################################

# Check for and remove *extreme* outliers

NachmanTransectsMetadata %>% ggplot(aes(x=Ear_Length_mm)) + geom_histogram(binwidth = 1)
# no clear extreme outliers for body weight


# Make models

lmELBW <- lm(Ear_Length_mm ~ Body_Weight_g + Absolute_Latitude, data = NachmanTransectsMetadata)
check_model(lmELBW)
shapiro.test(resid(lmELBW)) # not normally distributed

ggplot(data = NachmanTransectsMetadata, aes(x = Body_Weight_g, y = Ear_Length_mm)) +
  geom_point() +
  geom_smooth(method = "lm")

# sample size of lmTLBW resids
length(which(!is.na(resid(lmELBW))))



lmELBL <- lm(Ear_Length_mm ~ Body_Length_mm + Absolute_Latitude, data = NachmanTransectsMetadata)
check_model(lmELBL)
shapiro.test(resid(lmELBL)) # not normally distributed

ggplot(data = NachmanTransectsMetadata, aes(x = Body_Length_mm, y = Ear_Length_mm)) +
  geom_point() +
  geom_smooth(method = "lm")

# sample size of lmTLBL resids
length(which(!is.na(resid(lmELBL))))


compare_performance(lmELBW, lmELBL, rank = TRUE)

# body weight gives better model (just like with tail length)


# save EL x BW residuals
residsELBW <- lm(Ear_Length_mm ~ Body_Weight_g, data = NachmanTransectsMetadata,
                 na.action = na.exclude)
NachmanTransectsMetadata$Resids_ELBW <- resid(residsELBW)





################################################################################
# Stats for Allen's rule and Bergmann's rule
# All sexes
################################################################################

# Bergmann's rule
fullmodel.Berg <- lm(Body_Weight_g ~ Absolute_Latitude + Sex,
                     data = NachmanTransectsMetadata)
summary(fullmodel.Berg)

cor.Berg <- cor.test(x = NachmanTransectsMetadata$Absolute_Latitude,
                     y = NachmanTransectsMetadata$Body_Weight_g,
                     method = 'pearson')
cor.Berg

cor.Berg.2 <- Hmisc::rcorr(x = NachmanTransectsMetadata$Absolute_Latitude,
                           y = NachmanTransectsMetadata$Body_Weight_g,
                           type="pearson")
cor.Berg.2


# Allen's rule - tails
fullmodel.tail <- lm(Tail_Length_mm ~ Body_Weight_g + Absolute_Latitude + Sex,
                     data = NachmanTransects_filtered)
summary(fullmodel.tail)

cor.tail <- cor.test(x = NachmanTransects_filtered$Absolute_Latitude,
                     y = NachmanTransects_filtered$Resids_TLBW,
                     method = 'spearman')
cor.tail

cor.tail.2 <- Hmisc::rcorr(x = NachmanTransects_filtered$Absolute_Latitude,
                           y = NachmanTransects_filtered$Resids_TLBW,
                           type="spearman")
cor.tail.2


# Allen's rule - ears
fullmodel.ear <- lm(Ear_Length_mm ~ Body_Weight_g + Absolute_Latitude + Sex,
                    data = NachmanTransectsMetadata)
summary(fullmodel.ear)

cor.ear <- cor.test(x = NachmanTransectsMetadata$Absolute_Latitude,
                    y = NachmanTransectsMetadata$Resids_ELBW,
                    method = 'spearman')
cor.ear

cor.ear.2 <- Hmisc::rcorr(x = NachmanTransectsMetadata$Absolute_Latitude,
                          y = NachmanTransectsMetadata$Resids_ELBW,
                          type="spearman")
cor.ear.2


################################################################################
# Males only
################################################################################

Male_Bergmann <- NachmanTransectsMetadata %>%
  filter(Sex == "male")

fullmodel.Berg.Male <- lm(Body_Weight_g ~ Absolute_Latitude,
                          data = Male_Bergmann)
summary(fullmodel.Berg.Male)
#shapiro.test(resid(fullmodel.Berg.Male)) # normally distributed

cor.Berg.Male <- cor.test(x = Male_Bergmann$Absolute_Latitude,
                          y = Male_Bergmann$Body_Weight_g,
                          method = 'pearson')
cor.Berg.Male

cor.Berg.Male.2 <- Hmisc::rcorr(x = Male_Bergmann$Absolute_Latitude,
                                y = Male_Bergmann$Body_Weight_g,
                                type="pearson")
cor.Berg.Male.2


Male_Tail <- NachmanTransects_filtered %>%
  filter(Sex == "male")

fullmodel.Tail.Male <- lm(Tail_Length_mm ~ Body_Weight_g + Absolute_Latitude,
                          data = Male_Tail)
summary(fullmodel.Tail.Male)
#shapiro.test(resid(fullmodel.Tail.Male))

cor.Tail.Male <- cor.test(x = Male_Tail$Absolute_Latitude,
                          y = Male_Tail$Resids_TLBW,
                          method = 'spearman')
cor.Tail.Male

cor.Tail.Male.2 <- Hmisc::rcorr(x = Male_Tail$Absolute_Latitude,
                                y = Male_Tail$Resids_TLBW,
                                type="spearman")
cor.Tail.Male.2


fullmodel.Ear.Male <- lm(Ear_Length_mm ~ Body_Weight_g + Absolute_Latitude,
                         data = Male_Bergmann)
summary(fullmodel.Ear.Male)
#shapiro.test(resid(fullmodel.Ear.Male))

cor.Ear.Male <- cor.test(x = Male_Bergmann$Absolute_Latitude,
                         y = Male_Bergmann$Resids_ELBW,
                         method = 'spearman')
cor.Ear.Male

cor.Ear.Male.2 <- Hmisc::rcorr(x = Male_Bergmann$Absolute_Latitude,
                               y = Male_Bergmann$Resids_ELBW,
                               type="spearman")
cor.Ear.Male.2


################################################################################
# Females only
################################################################################

Female_Bergmann <- NachmanTransectsMetadata %>%
  filter(Sex == "female")

fullmodel.Berg.Female <- lm(Body_Weight_g ~ Absolute_Latitude, data = Female_Bergmann)
summary(fullmodel.Berg.Female)
#shapiro.test(resid(fullmodel.Berg.Female)) # normally distributed

cor.Berg.Female <- cor.test(x = Female_Bergmann$Absolute_Latitude,
                            y = Female_Bergmann$Body_Weight_g,
                            method = 'pearson')
cor.Berg.Female

cor.Berg.Female.2 <- Hmisc::rcorr(x = Female_Bergmann$Absolute_Latitude,
                                  y = Female_Bergmann$Body_Weight_g,
                                  type="pearson")
cor.Berg.Female.2


Female_Tail <- NachmanTransects_filtered %>%
  filter(Sex == "female")

fullmodel.Tail.Female <- lm(Tail_Length_mm ~ Body_Weight_g + Absolute_Latitude,
                            data = Female_Tail)
summary(fullmodel.Tail.Female)
#shapiro.test(resid(fullmodel.Tail.Female))

cor.Tail.Female <- cor.test(x = Female_Tail$Absolute_Latitude,
                            y = Female_Tail$Resids_TLBW,
                            method = 'spearman')
cor.Tail.Female

cor.Tail.Female.2 <- Hmisc::rcorr(x = Female_Tail$Absolute_Latitude,
                                  y = Female_Tail$Resids_TLBW,
                                  type="spearman")
cor.Tail.Female.2


Female_Ear <- NachmanTransectsMetadata %>%
  filter(Sex == "female")

fullmodel.Ear.Female <- lm(Ear_Length_mm ~ Body_Weight_g + Absolute_Latitude,
                           data = Female_Ear)
summary(fullmodel.Ear.Female)
#shapiro.test(resid(fullmodel.Ear.Female))

cor.Ear.Female <- cor.test(x = Female_Ear$Absolute_Latitude,
                           y = Female_Ear$Resids_ELBW,
                           method = 'spearman')
cor.Ear.Female

cor.Ear.Female.2 <- Hmisc::rcorr(x = Female_Ear$Absolute_Latitude,
                                 y = Female_Ear$Resids_ELBW,
                                 type="spearman")
cor.Ear.Female.2
