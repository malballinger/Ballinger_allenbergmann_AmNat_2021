#!/usr/bin/env Rscript --vanilla

##############################################################

# Author: Mallory A. Ballinger

# This script plots reaction norms of BMI of New York
# and Brazil mice across environments (i.e. common garden experiment #2.)
# This script generates Fig. S4 in Ballinger_AmNat_2021.


##############################################################
# Required packages
##############################################################

rm(list = ls()) # clears R's environment
library(tidyverse)
library(here)
library(cowplot)
library(glue)
library(ggtext)

set.seed(19910118) # so that jitter plots stay in same jittered positions


##############################################################
# Import data
##############################################################

PostDissectionMetaData <- read_csv(here("data/processed/PostDissectionMetaData.csv")) %>%
  select(-1) %>% # removes first column
  mutate(Sex = fct_relevel(Sex, "Male", "Female")) %>% # put males before females
  mutate(Population = fct_relevel(Population, "New York", "Brazil")) %>% # put evolved cold pop first
  mutate(Line = fct_relevel(Line, "SARA", "SARB", "MANA", "MANB")) %>% # put evolved cold lines first
  mutate(Environment = fct_relevel(Environment, "Warm", "Cold" )) # put warm before cold (evolved before plasticity)


##############################################################
# Get values from statistical analyses
##############################################################

# Refer to ./model_RXNs.R for model comparisons and statistical analyses

# > car::Anova(lmer(BMI_kg_m2 ~ Sex * Population * Environment + (1|Line),
#                   +                 data = PostDissectionMetaData), type = "III")
# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: BMI_kg_m2
# Chisq Df Pr(>Chisq)    
# (Intercept)                685.5193  1  < 2.2e-16 ***
# Sex                          7.3520  1   0.006699 ** 
# Population                   0.5018  1   0.478721    
# Environment                  1.2811  1   0.257699    
# Sex:Population               0.5605  1   0.454068    
# Sex:Environment              0.6007  1   0.438295    
# Population:Environment       0.9289  1   0.335138    
# Sex:Population:Environment   0.1696  1   0.680472    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

BMI_deets <- ("&#42;sex")


##############################################################
# Plot sex-specific BMI plot
##############################################################

# Sex-specific datasets
MaleData <- PostDissectionMetaData %>%
  filter(Sex == "Male")

FemaleData <- PostDissectionMetaData %>%
  filter(Sex == "Female")


BMIrxnF <-
  ggplot(data=FemaleData, aes(x=Environment, y=BMI_kg_m2)) +
  geom_boxplot(aes(fill = Population), position = position_dodge(width=0.25), alpha=0.8, size = 0.5, outlier.shape = NA, notch = FALSE, coef=0) +
  stat_summary(
    fun = median,
    geom = 'line',
    aes(group = Population, color = Population),
    position = position_dodge(width = 0.25) , size=1.5, alpha = 1.2, linetype = "dashed") +
  geom_point(aes(fill = Population), position = position_jitterdodge(jitter.width = 0.15, dodge.width=0.25), alpha=5, size=2, shape=21, color = "white") +
  #annotate("text", x = 0.75, y=25, label="Males", family = "Palatino") +
  scale_color_manual(values=rep(c("dodgerblue4", "goldenrod1"))) +
  scale_fill_manual(values=rep(c("dodgerblue4", "goldenrod1"))) +
  #scale_x_discrete("Environment", expand=c(0.075,0.5)) +
  #guides(color=guide_legend(override.aes=list(fill=NA)),
  # linetype=guide_legend(override.aes = list(fill=NA))) +# removes gray shading from legend
  #scale_y_continuous(breaks = seq(from=8, to=24, by=4), labels = seq(from=8, to=24, by=4), limits = c(8,25)) +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_markdown(margin = margin(r = 10), size = 11, family = "Palatino"),
        axis.text.x = element_text(size = 10, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.position = c(0.047, 0.96),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(0.35, "cm"),
        legend.key.width = unit(0.35, "cm"),
        legend.text = element_text(size=8, family = "Palatino"),
        legend.title = element_blank(),
        plot.tag = element_text(family = "Palatino", size = 8, face = "italic"),
        plot.tag.position = c(0.2,1.015),
        plot.title = element_text(size = 8, face = "italic", hjust = 0.05, vjust = -3, family = "Palatino"),
        plot.title.position = "panel",
        plot.margin = unit(c(0.5, -0.45, 0.5, 0.3), "cm")) +
  labs(x = "",
       y = "Body Mass Index (kg/m<sup>2</sup>)",
       title = "Females")


BMIrxnM <-
  ggplot(data=MaleData, aes(x=Environment, y=BMI_kg_m2)) +
  geom_boxplot(aes(fill = Population), position = position_dodge(width=0.25), alpha=0.8, size = 0.5, outlier.shape = NA, notch = FALSE, coef=0) +
  stat_summary(
    fun = median,
    geom = 'line',
    aes(group = Population, color = Population),
    position = position_dodge(width = 0.25) , size=1.5, alpha = 1.2, linetype = "dashed") +
  geom_point(aes(fill = Population), position = position_jitterdodge(jitter.width = 0.15, dodge.width=0.25), alpha=5, size=2, shape=21, color = "white") +
  #annotate("text", x = 2.4, y=25, label="****sex\n*pop", family = "Palatino", fontface = 3, size = 3, hjust = 1) +
  scale_color_manual(values=rep(c("dodgerblue4", "goldenrod1"))) +
  scale_fill_manual(values=rep(c("dodgerblue4", "goldenrod1"))) +
  #scale_x_discrete("Environment", expand=c(0.075,0.5)) +
  #scale_y_continuous(breaks = seq(from=1.2, to=3, by=4), labels = seq(from=8, to=24, by=4), limits = c(8,25)) +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10), size = 10, family = "Palatino"),
        axis.text.x = element_text(size = 10, color = "black", family = "Palatino"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "none",
        plot.tag = element_markdown(family = "Palatino", size = 8, face = "italic", hjust = 1),
        plot.tag.position = c(0.96,0.89),
        plot.title = element_text(size = 8, face = "italic", hjust = 0.05, vjust = -3, family = "Palatino"),
        plot.title.position = "panel",
        plot.margin = unit(c(0.5, 0.5, 0.5, -0.42), "cm")) + # top, right, bottom, left (0.5, 0.5, 0.1, -0.5)
  labs(x = "",
       y = "",
       tag = BMI_deets,
       title = "Males")


BMI <- cowplot::plot_grid(BMIrxnF, BMIrxnM, align = "h", label_fontfamily = "Palatino",
                         label_size = 12, label_x = 0.05, hjust = 0)


#ggsave("results/figures/RXNs_BMI.tiff", height = 3.5, width = 6, compression = "lzw")
ggsave("results/figures/RXNs_BMI.pdf", height = 3.5, width = 6)