#!/usr/bin/env Rscript --vanilla

################################################################################

# Author: Mallory A. Ballinger

# This script models body mass and extremity length from wild-caught house mice
# collected across North and South America. Data are from VertNet.org and were
# cleaned using the script ./clean_VertNetMetadata.R.
# This script generates statistical analyses for Ballinger_AmNat_2021.


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
library(qqplotr)

################################################################################
# Import data               
################################################################################

VertNetMetadata <- read_csv(here("data/processed/VertNetMetadata_Mus_2021-03-18.csv")) %>%
  select(-1)

# get sample size of each column
#colSums(!is.na(VertNetMetadata))


################################################################################
# Data and Model testing
# *Bergmann's rule*
################################################################################

# Check for and remove *extreme* outliers

VertNetMetadata %>% ggplot(aes(x=Body_Weight_g)) + geom_histogram(binwidth = 1)

BWOutliers <- VertNetMetadata %>%
  summarise(VertNetMetadata, meanBW = mean(Body_Weight_g, na.rm = TRUE),
            sdBW = sd(Body_Weight_g, na.rm = TRUE)) %>%
  filter(Body_Weight_g < (meanBW - 3.5*sdBW) | Body_Weight_g > (meanBW + 3.5*sdBW))

# no clear extreme outliers above or below 3.5 stdev from mean to remove

# get sample size for Body Weight
length(which(!is.na(VertNetMetadata$Body_Weight_g)))


# Check if data are normally distributed

lmBW <- lm(Body_Weight_g ~ Absolute_Latitude, data = VertNetMetadata)
check_model(lmBW)
shapiro.test(resid(lmBW)) # not normally distributed





################################################################################
# Data and Model testing
# *Allen's rule* - tail length
################################################################################

# Check for and remove *extreme* outliers

VertNetMetadata %>% ggplot(aes(x=Tail_Length_mm)) + geom_histogram(binwidth = 1)

TLOutliers <- VertNetMetadata %>%
  summarise(VertNetMetadata, meanTL = mean(Tail_Length_mm, na.rm = TRUE),
            sdTL = sd(Tail_Length_mm, na.rm = TRUE)) %>%
  filter(Tail_Length_mm < (meanTL - 3.5*sdTL) | Tail_Length_mm > (meanTL + 3.5*sdTL))

VertNet_filtered <- VertNetMetadata %>%
  mutate(Tail_Length_mm = ifelse(Tail_Length_mm < 20, NA, Tail_Length_mm),
         Tail_Length_mm = ifelse(Tail_Length_mm > 120, NA, Tail_Length_mm)) %>%
  mutate(RelativeTail = (Tail_Length_mm) / (Body_Weight_g))

# any tail shorter than 20mm and longer than 120mm are extreme outliers (>3.5 stdev from mean)

VertNet_filtered %>% ggplot(aes(x = Tail_Length_mm)) + geom_histogram(binwidth = 1)



# Make model
lmRelTail <- lm(RelativeTail ~ Absolute_Latitude, data = VertNet_filtered)
check_model(lmRelTail)
shapiro.test(resid(lmRelTail)) # not normally distributed

# sample size of lmTLBW resids
length(which(!is.na(VertNet_filtered$RelativeTail)))






################################################################################
# Data and Model testing
# *Allen's rule* - ear length
################################################################################

# Check for and remove *extreme* outliers

VertNetMetadata %>% ggplot(aes(x=Ear_Length_mm)) + geom_histogram(binwidth = 1)

ELOutliers <- VertNetMetadata %>%
  summarise(VertNetMetadata, meanEL = mean(Ear_Length_mm, na.rm = TRUE),
            sdEL = sd(Ear_Length_mm, na.rm = TRUE)) %>%
  filter(Ear_Length_mm < (meanEL - 3.5*sdEL) | Ear_Length_mm > (meanEL + 3.5*sdEL))

# any ear longer than 30 mm is an extreme outlier (>3.5 stdev from mean)

VertNet_filtered_2 <- VertNetMetadata %>%
  mutate(Ear_Length_mm = ifelse(Ear_Length_mm > 30, NA, Ear_Length_mm)) %>%
  mutate(RelativeEar = (Ear_Length_mm) / (Body_Weight_g))

VertNet_filtered_2 %>% ggplot(aes(x = Ear_Length_mm)) + geom_histogram(binwidth = 1)


# Make models

lmRelEar <- lm(RelativeEar ~ Absolute_Latitude, data = VertNet_filtered_2)
check_model(lmRelEar)
shapiro.test(resid(lmRelEar)) # not normally distributed

# sample size of lmTLBW resids
length(which(!is.na(VertNet_filtered_2$RelativeEar)))





################################################################################
# Stats for Allen's rule and Bergmann's rule
# All sexes
################################################################################

# Bergmann's rule
fullmodel.Berg <- lm(Body_Weight_g ~ Sex + Absolute_Latitude,
                     data = VertNetMetadata)
summary(fullmodel.Berg)

cor.Berg <- cor.test(x = VertNetMetadata$Absolute_Latitude,
                     y = VertNetMetadata$Body_Weight_g,
                     method = 'spearman')
cor.Berg

cor.Berg.2 <- rcorr(x = VertNetMetadata$Absolute_Latitude,
                    y = VertNetMetadata$Body_Weight_g,
                    type="spearman")
cor.Berg.2



# Allen's rule - tails
fullmodel.tail <- lm(RelativeTail ~ Sex + Absolute_Latitude,
                     data = VertNet_filtered)
summary(fullmodel.tail)
#check_model(fullmodel.tail)
#shapiro.test(resid(fullmodel.tail)) # not normally distributed

cor.tail <- cor.test(x = VertNet_filtered$Absolute_Latitude,
                     y = VertNet_filtered$RelativeTail,
                     method = 'spearman')
cor.tail


# Allen's rule - ears
fullmodel.ear <- lm(RelativeEar ~ Sex + Absolute_Latitude,
                     data = VertNet_filtered_2)
summary(fullmodel.ear)
#check_model(fullmodel.ear)
#shapiro.test(resid(fullmodel.ear)) # not normally distributed

cor.ear <- cor.test(x = VertNet_filtered_2$Absolute_Latitude,
                    y = VertNet_filtered_2$RelativeEar,
                    method = 'spearman')
cor.ear


################################################################################
# Males only
################################################################################

Male_Bergmann <- VertNetMetadata %>%
  filter(Sex == "male")

fullmodel.Berg.Male <- lm(rank(Body_Weight_g) ~ Absolute_Latitude,
                          data = Male_Bergmann)
summary(fullmodel.Berg.Male)
#check_model(fullmodel.Berg.Male)
#shapiro.test(resid(fullmodel.Berg.Male)) # not normally distributed

cor.Berg.Male <- cor.test(x = Male_Bergmann$Absolute_Latitude,
                          y = Male_Bergmann$Body_Weight_g,
                          method = "spearman", exact = FALSE,
                          adjust="fdr", alpha=0.5)
cor.Berg.Male

cor.Berg.Male.2 <- Hmisc::rcorr(x = Male_Bergmann$Absolute_Latitude,
                                y = Male_Bergmann$Body_Weight_g,
                                type='spearman')
cor.Berg.Male.2


Male_Tail <- VertNet_filtered %>%
  filter(Sex == "male")

fullmodel.Tail.Male <- lm(RelativeTail ~ Absolute_Latitude,
                          data = Male_Tail)
summary(fullmodel.Tail.Male)
#check_model(fullmodel.Tail.Male)
#shapiro.test(resid(fullmodel.Tail.Male)) # not normally distributed

cor.Tail.Male <- cor.test(x = Male_Tail$Absolute_Latitude,
                          y = Male_Tail$RelativeTail,
                          method = "spearman",
                          adjust="fdr", alpha=0.5)
cor.Tail.Male

cor.Tail.Male.2 <- Hmisc::rcorr(x = Male_Tail$Absolute_Latitude,
                                y = Male_Tail$Resids_TLBW,
                                type="spearman")
cor.Tail.Male.2


Male_Ear <- VertNet_filtered_2 %>%
  filter(Sex == "male")

fullmodel.Ear.Male <- lm(RelativeEar ~ Absolute_Latitude,
                          data = Male_Ear)
summary(fullmodel.Ear.Male)
#check_model(fullmodel.Ear.Male)
#shapiro.test(resid(fullmodel.Ear.Male)) # not normally distributed

cor.Ear.Male <- cor.test(x = Male_Ear$Absolute_Latitude,
                         y = Male_Ear$RelativeEar,
                         method = 'spearman')
cor.Ear.Male

cor.Ear.Male.2 <- Hmisc::rcorr(x = Male_Ear$Absolute_Latitude,
                               y = Male_Ear$Resids_ELBW,
                               type="spearman")
cor.Ear.Male.2


################################################################################
# Females only
################################################################################

Female_Bergmann <- VertNetMetadata %>%
  filter(Sex == "female")

fullmodel.Berg.Female <- lm(rank(Body_Weight_g) ~ Absolute_Latitude, data = Female_Bergmann)
summary(fullmodel.Berg.Female)
#check_model(fullmodel.Berg.Female)
#shapiro.test(resid(fullmodel.Berg.Female)) # not normally distributed

cor.Berg.Female <- cor.test(x = Female_Bergmann$Absolute_Latitude,
                            y = Female_Bergmann$Body_Weight_g,
                            method = 'spearman')
cor.Berg.Female

cor.Berg.Female.2 <- Hmisc::rcorr(x = Female_Bergmann$Absolute_Latitude,
                                  y = Female_Bergmann$Body_Weight_g,
                                  type="spearman")
cor.Berg.Female.2


Female_Tail <- VertNet_filtered %>%
  filter(Sex == "female")

fullmodel.Tail.Female <- lm(RelativeTail ~ Absolute_Latitude,
                          data = Female_Tail)
summary(fullmodel.Tail.Female)
#check_model(fullmodel.Tail.Female)
#shapiro.test(resid(fullmodel.Tail.Female)) # not normally distributed

cor.Tail.Female <- cor.test(x = Female_Tail$Absolute_Latitude,
                            y = Female_Tail$RelativeTail,
                            method = 'spearman')
cor.Tail.Female

cor.Tail.Female.2 <- Hmisc::rcorr(x = Female_Tail$Absolute_Latitude,
                                  y = Female_Tail$Resids_TLBW,
                                  type="spearman")
cor.Tail.Female.2


Female_Ear <- VertNet_filtered_2 %>%
  filter(Sex == "female")

fullmodel.Ear.Female <- lm(RelativeEar ~ Absolute_Latitude,
                         data = Female_Ear)
summary(fullmodel.Ear.Female)
#check_model(fullmodel.Ear.Female)
#shapiro.test(resid(fullmodel.Ear.Female)) # not normally distributed

cor.Ear.Female <- cor.test(x = Female_Ear$Absolute_Latitude,
                           y = Female_Ear$RelativeEar,
                           method = 'spearman')
cor.Ear.Female

cor.Ear.Female.2 <- Hmisc::rcorr(x = Female_Ear$Absolute_Latitude,
                                 y = Female_Ear$Resids_ELBW,
                                 type="spearman")
cor.Ear.Female.2



################################################################################
# Adult Males only
################################################################################

Male_Adult_Bergmann <- Male_Bergmann %>%
  filter(Lifestage == "adult")

fullmodel.Berg.Male.Adult <- lm(Body_Weight_g ~ Absolute_Latitude,
                          data = Male_Adult_Bergmann)
summary(fullmodel.Berg.Male.Adult)
#check_model(fullmodel.Berg.Male.Adult)
#shapiro.test(resid(fullmodel.Berg.Male.Adult)) # not normally distributed

cor.Berg.Male.Adult <- cor.test(x = Male_Adult_Bergmann$Absolute_Latitude,
                                y = Male_Adult_Bergmann$Body_Weight_g,
                                method = 'spearman')
cor.Berg.Male.Adult


Male_Adult_Tail <- Male_Tail %>%
  filter(Lifestage == "adult")

fullmodel.Tail.Male.Adult <- lm(Tail_Length_mm ~ Absolute_Latitude,
                          data = Male_Adult_Tail)
summary(fullmodel.Tail.Male.Adult)
#check_model(fullmodel.Tail.Male.Adult)
#shapiro.test(resid(fullmodel.Tail.Male.Adult)) # not normally distributed

cor.Tail.Male.Adult <- cor.test(x = Male_Adult_Tail$Absolute_Latitude,
                                y = Male_Adult_Tail$RelativeTail,
                                method = 'spearman')
cor.Tail.Male.Adult


Male_Adult_Ear <- Male_Ear %>%
  filter(Lifestage == "adult")

fullmodel.Ear.Male.Adult <- lm(Ear_Length_mm ~ Absolute_Latitude,
                         data = Male_Adult_Ear)
summary(fullmodel.Ear.Male.Adult)
#check_model(fullmodel.Ear.Male.Adult)
#shapiro.test(resid(fullmodel.Ear.Male.Adult)) # not normally distributed

cor.Ear.Male.Adult <- cor.test(x = Male_Adult_Ear$Absolute_Latitude,
                               y = Male_Adult_Ear$RelativeEar,
                               method = 'spearman')
cor.Ear.Male.Adult





################################################################################
# Adult Males only - Relative Extremities
################################################################################

Male_Bergmann_Adults <- Male_Bergmann %>%
  filter(Lifestage == "adult")

fullmodel.Berg.Male.Adult <- lm(Body_Weight_g ~ Absolute_Latitude,
                          data = Male_Bergmann_Adults)
summary(fullmodel.Berg.Male.Adult)
#check_model(fullmodel.Berg.Male.Adult)
#shapiro.test(resid(fullmodel.Berg.Male.Adult)) # not normally distributed

cor.Berg.Male.Adult <- cor.test(x = Male_Bergmann_Adults$Absolute_Latitude,
                          y = Male_Bergmann_Adults$Body_Weight_g,
                          method = "spearman")
cor.Berg.Male.Adult



Male_Tail_Adults <- Male_Tail %>%
  filter(Lifestage == "adult")

fullmodel.Tail.Male.Adult <- lm(RelativeTail ~ Absolute_Latitude,
                          data = Male_Tail_Adults)
summary(fullmodel.Tail.Male.Adult)
#check_model(fullmodel.Tail.Male.Adult)
#shapiro.test(resid(fullmodel.Tail.Male.Adult)) # not normally distributed

cor.Tail.Male.Adult <- cor.test(x = Male_Tail_Adults$Absolute_Latitude,
                          y = Male_Tail_Adults$RelativeTail,
                          method = "spearman")
cor.Tail.Male.Adult



Male_Ear_Adults <- Male_Ear %>%
  filter(Lifestage == "adult")

fullmodel.Ear.Male.Adult <- lm(RelativeEar ~ Absolute_Latitude,
                         data = Male_Ear_Adults)
summary(fullmodel.Ear.Male.Adult)
#check_model(fullmodel.Ear.Male.Adult)
#shapiro.test(resid(fullmodel.Ear.Male.Adult)) # not normally distributed

cor.Ear.Male.Adult <- cor.test(x = Male_Ear_Adults$Absolute_Latitude,
                         y = Male_Ear_Adults$RelativeEar,
                         method = 'spearman')
cor.Ear.Male.Adult
