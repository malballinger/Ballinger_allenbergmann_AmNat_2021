#!/usr/bin/env Rscript --vanilla

##############################################################

# Author: Mallory A. Ballinger

# This script plots reaction norms of *relative* extremity lengths of New York
# and Brazil mice across environments (i.e. common garden experiment #2.)
# This script generates Fig. 6 in Ballinger_AmNat_2021.


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
  mutate(Environment = fct_relevel(Environment, "Warm", "Cold" )) %>% # put warm before cold (evolved before plasticity)
  mutate(RelativeTail = ((Final_Tail_Length_mm) / (Body_Weight_g)))





##############################################################
# Calculate residuals for plotting
##############################################################

# Based on outlier tests (see 'code/model_RXNs_relative.R'), any ear length
# greater than 17mm is an extreme outlier

PostDissection_filtered <- PostDissectionMetaData %>%
  mutate(Ear_Length_mm = ifelse(Ear_Length_mm > 17, NA, Ear_Length_mm)) %>%
  mutate(RelativeEar = ((Ear_Length_mm) / (Body_Weight_g)))





##############################################################
# Get values from statistical analyses
##############################################################

# Refer to ./model_RXNs_relative.R for model comparisons and statistical analyses


# > car::Anova(lmer(RelativeTail ~ Sex * Population * Environment + (1|Line),
#                   +                 data = PostDissectionMetaData), type = "III")
# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: RelativeTail
# Chisq Df Pr(>Chisq)    
# (Intercept)                209.0040  1  < 2.2e-16 ***
# Sex                         42.4413  1  7.283e-11 ***
# Population                   6.5394  1    0.01055 *  
# Environment                 25.5928  1  4.216e-07 ***
# Sex:Population               0.0800  1    0.77731    
# Sex:Environment              0.0731  1    0.78681    
# Population:Environment       3.7085  1    0.05414 .  
# Sex:Population:Environment   0.3149  1    0.57468  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

TL_deets <- ("pop x env (*P* = 0.05)<br>\
              &#42;sex<br>\
              &#42;pop<br>\
              &#42;env")



# > car::Anova(lmer(RelativeEar ~ Sex * Population * Environment + (1|Line),
# data = PostDissection_filtered), type = "III")
# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: RelativeEar
# Chisq Df Pr(>Chisq)    
# (Intercept)                211.2918  1  < 2.2e-16 ***
# Sex                         45.6153  1  1.439e-11 ***
# Population                   3.7165  1    0.05388 .  
# Environment                 20.0683  1  7.473e-06 ***
# Sex:Population               0.0294  1    0.86382    
# Sex:Environment              0.6905  1    0.40599    
# Population:Environment       0.2604  1    0.60983    
# Sex:Population:Environment   0.0652  1    0.79848  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

EL_deets <- glue("pop (*P* = 0.05)<br>\
                  &#42;sex<br>\
                  &#42;env")





##############################################################
# Plot rxn norm plots (tail length & ear length)
##############################################################


TLrxn <-
  ggplot(data=PostDissectionMetaData, aes(x=Environment, y=RelativeTail)) +
  geom_boxplot(aes(fill = Population), position = position_dodge(width=0.25), alpha=0.8, size = 0.5, outlier.shape = NA, notch = FALSE, coef=0) +
  stat_summary(
    fun = median,
    geom = 'line',
    aes(group = Population, color = Population),
    position = position_dodge(width = 0.25) , size=1.5, alpha = 1.2, linetype = "dashed") +
  geom_point(aes(fill = Population), position = position_jitterdodge(jitter.width = 0.15, dodge.width=0.25), alpha=5, size=2.25, shape=21, color = "white") +
  scale_color_manual(values=rep(c("dodgerblue4", "goldenrod1"))) +
  scale_fill_manual(values=rep(c("dodgerblue4", "goldenrod1"))) +
  scale_y_continuous(breaks = seq(from=3, to=8.5, by=1.5), labels = seq(from=3, to=8.5, by=1.5), limits = c(3,8.5)) +
  theme_bw() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10), size = 12, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 12, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 11, color = "black", family = "Palatino"),
        legend.position = c(0.14, 0.96),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(0.35, "cm"),
        legend.key.width = unit(0.35, "cm"),
        legend.text = element_text(size=10, family = "Palatino"),
        legend.title = element_blank(),
        plot.tag = element_markdown(family = "Palatino", size = 10, face = "italic", hjust = 1),
        plot.tag.position = c(0.97,0.915),
        plot.title = element_markdown(family = "Palatino"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  labs(x = "",
       y = "Relative Tail Length",
       tag = TL_deets)


ELrxn <-
  ggplot(data=PostDissection_filtered, aes(x=Environment, y=RelativeEar)) +
  geom_boxplot(aes(fill = Population), position = position_dodge(width=0.25), alpha=0.8, size = 0.5, outlier.shape = NA, notch = FALSE, coef=0) +
  stat_summary(
    fun = median,
    geom = 'line',
    aes(group = Population, color = Population),
    position = position_dodge(width = 0.25) , size=1.5, alpha = 1.2, linetype = "dashed") +
  geom_point(aes(fill = Population), position = position_jitterdodge(jitter.width = 0.15, dodge.width=0.25), alpha=5, size=2.25, shape=21, color = "white") +
  scale_color_manual(values=rep(c("dodgerblue4", "goldenrod1"))) +
  scale_fill_manual(values=rep(c("dodgerblue4", "goldenrod1"))) +
  scale_y_continuous(breaks = seq(from=0.5, to=1.5, by=0.25), labels = seq(from=0.5, to=1.5, by=0.25), limits = c(0.5,1.5)) +
  theme_bw() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10), size = 12, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 12, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 11, color = "black", family = "Palatino"),
        legend.position = 'none',
        plot.tag = element_markdown(family = "Palatino", size = 10, face = "italic", hjust = 1),
        plot.tag.position = c(0.97,0.92),
        plot.title = element_markdown(family = "Palatino"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  labs(x = "",
       y = "Relative Ear Length",
       tag = EL_deets)


Extremities <- cowplot::plot_grid(TLrxn, ELrxn, ncol = 2, nrow = 1, labels = c('A)', 'B)'), label_fontfamily = "Palatino",
                   align = 'h', label_size = 14, label_x = 0.05, hjust = 0)

ggsave("results/figures/RXNs_Extremities_relative.tiff", height = 4, width = 8.5, compression = "lzw")
ggsave("results/figures/RXNs_Extremities_relative.pdf", height = 4, width = 8.5)
