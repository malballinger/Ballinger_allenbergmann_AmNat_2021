#!/usr/bin/env Rscript --vanilla

# name: weekly_phenotypes_graphed.R
#
# Plot evolved and plastic differences in weekly body weights and tail lengths
#
# input:
#       - data/raw/WeeklyMetaData
# output:
#       - XXXX

# Clear R and load libraries
rm(list = ls())
library(tidyverse)
library(here)

# Read in data
WeeklyMetaData <- read_csv(here("data/raw/WeeklyMetaData.csv")) %>%
  select(-DOB, -DateMeasured) %>%
  filter(Population == "BRAZIL" | Population == "NEW_YORK") %>%
  mutate(PopByEnv = paste(Population, Environment))

# Clean data of any weird spaces, etc.
WeeklyMetaData[] <- lapply(WeeklyMetaData, function(x) if(is.factor(x)) factor(x) else x)


#### Plot all lines by sex ####
# Males only
MaleData <- WeeklyMetaData %>%
  filter(Sex =="M")

MaleBodyWeight <-
  ggplot(data=MaleData, aes(x=Age_weeks, y= BodyWeight_g, color=Population)) +
  geom_smooth(aes(linetype=Environment, color=Population), size=0.5, method="loess") +
  geom_point(position = position_dodge(0.1), size=1, alpha=0.5) +
  theme_classic()
MaleBodyWeight

MaleTailLength <-
  ggplot(data=MaleData, aes(x=Age_weeks, y= TailLength_mm, color=Population)) +
  geom_smooth(aes(linetype=Environment, color=Population), size=0.5, method="loess") +
  geom_point(position = position_dodge(0.1), size=1, alpha=0.5) +
  theme_classic()
MaleTailLength