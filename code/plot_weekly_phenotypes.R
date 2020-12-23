#!/usr/bin/env Rscript --vanilla

# name: weekly_phenotypes_graphed.R
#
# Plot evolved and plastic differences in weekly body weights and tail lengths
#
# input:
#       - data/raw/WeeklyMetaData
# output:
#       - results/figures/WeeklyPhenotypes

# Clear R environment and load libraries
rm(list = ls())
library(tidyverse)
library(here)
library(cowplot)


# Read in data
WeeklyMetaData <- read_csv(here("data/raw/WeeklyMetaData.csv")) %>%
  select(-DOB, -DateMeasured) %>%
  filter(Population == "BRAZIL" | Population == "NEW_YORK") %>%
  #filter(Age_weeks > 3) %>%  #NY lines sometimes were weaned & measured slightly before 3 weeks
  filter(Age_weeks < 12) %>%
  mutate(Sex = fct_recode(Sex, "Females" = "F", "Males" = "M")) %>% # spells out males and females
  mutate(Environment = fct_relevel(Environment,
                                   "RT", "COLD" )) # puts RT before COLD

# Clean data of any weird spaces, etc.
WeeklyMetaData[] <- lapply(WeeklyMetaData, function(x) if(is.factor(x)) factor(x) else x)


#### Plot reaction norms of BW and TL at weaning/3.5 weeks and end of experiment/11 weeks ####
# These plots will be combined with the weekly plots - data from PD sheet


#### Plot all lines by sex ####
# Males only
# MaleData <- WeeklyMetaData %>%
#   filter(Sex =="M")

BodyWeight <-
  ggplot(data=WeeklyMetaData, aes(x=Age_weeks, y= BodyWeight_g, color=Population)) +
  geom_smooth(aes(linetype=Environment, color=Population), size=1, method="loess") + #std error
  geom_point(position = position_dodge(0.1), size=1.5, alpha=0.5) +
  scale_color_manual(values=c("goldenrod1","dodgerblue4")) +
  coord_cartesian(xlim = c(3,12), ylim = c(3,25)) +
  facet_grid(. ~ Sex) +
  theme_half_open(12) +
  panel_border() +
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 20), size = 15, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 20), size = 15, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold", color = "black"),
        axis.text.y = element_text(size = 12, face = "bold", color = "black"),
        #legend.title = element_blank(),
        legend.position = "none",
       # legend.key.size = unit(1.5, "cm"),
       # legend.text = element_text(size=15),
        #legend.key = element_rect(fill = "transparent"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
  labs(x = "Age (weeks)", y = "Body Weight (g)")

TailLength <-
  ggplot(data=WeeklyMetaData, aes(x=Age_weeks, y=TailLength_mm, color=Population)) +
  geom_smooth(aes(linetype=Environment, color=Population), size=1, method="loess") + #std error
  geom_point(position = position_dodge(0.1), size=1.5, alpha=0.5) +
  scale_color_manual(values=c("goldenrod1","dodgerblue4")) +
  coord_cartesian(xlim = c(3,12), ylim = c(45,85)) +
  facet_grid(. ~ Sex) +
  theme_half_open(12) +
  panel_border() +
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 20), size = 15, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 20), size = 15, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold", color = "black"),
        axis.text.y = element_text(size = 12, face = "bold", color = "black"),
        #legend.title = element_blank(),
        legend.position = "none",
        # legend.key.size = unit(1.5, "cm"),
        # legend.text = element_text(size=15),
        #legend.key = element_rect(fill = "transparent"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
  labs(x = "Age (weeks)", y = "Tail Length (mm)")

plot_grid(BodyWeight, TailLength, labels = c('A', 'B'), ncol = 2, nrow = 2)


#### Load PostDissection datasheet ####
# Read in data
PDMetaData <- read_csv(here("data/raw/PostDissection.csv")) %>%
  select(Population, Generation, Line, Mouse_ID, Sex, Environment, BodyWeight_g, WeaningBodyWeight_g, WeaningTailLength_mm, StartExpBodyWeight_g, StartExpTailLength_mm, FinalTailLength_mm) %>%
  filter(Population == "BRAZIL" | Population == "NEW_YORK") %>%
  mutate(Sex = fct_recode(Sex, "Females" = "F", "Males" = "M")) %>% # spells out males and females
  mutate(Environment = fct_relevel(Environment,
                                   "RT", "COLD" )) # puts RT before COLD

# Clean data of any weird spaces, etc.
PDMetaData[] <- lapply(PDMetaData, function(x) if(is.factor(x)) factor(x) else x)