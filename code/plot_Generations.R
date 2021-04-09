#!/usr/bin/env Rscript --vanilla

##############################################################
# Author: Mallory A. Ballinger
# Script first created: 24-Feb-2021
# Script last updated:  09-Apr-2021


# This script plots sex-specific body weight and extremity lengths of New York mice
# and Brazil mice across generations (i.e. common garden experiment #1).
# This script generates Figure 2 in Ballinger_et_al_2021_AmNat.


##############################################################
# Required packages
##############################################################

rm(list = ls()) # clear R's environment
library(tidyverse)
library(here)
library(ggridges)
library(cowplot)
library(car)
library(ggtext)
library(glue)
library(scales)


set.seed(19910118) # so that jitter plots stay in same jittered positions
##############################################################
# Import data
##############################################################

GenerationMetaData <- read_csv(here("data/processed/GenerationColonyData.csv")) %>%
  select(-GUID, -Collector_ID, -Age_days, -Notes) %>% select(-1) %>%
  mutate(Sex = fct_relevel(Sex, "Male", "Female")) %>% # put males before females
  mutate(Population = fct_relevel(Population, "New York", "Brazil")) # put evolved cold pop first

# get sample size of each column
#colSums(!is.na(GenerationMetaData))





##############################################################
# Apply filtering and calculate residuals
##############################################################

# Based on outlier tests (see 'code/model_Generations.R'), any tail length
# less than 50mm is an extreme outlier

Generation_filtered <- GenerationMetaData %>%
  mutate(Tail_Length_mm = ifelse(Tail_Length_mm < 50, NA, Tail_Length_mm))

# save TL x BW residuals
residsTLBW <- lm(Tail_Length_mm ~ Body_Weight_g, data = Generation_filtered,
                 na.action = na.exclude)
Generation_filtered$Resids_TLBW <- resid(residsTLBW)


# Based on outlier tests (see 'code/model_Generations.R'), any ear length
# less than 8mm is an extreme outlier

Generation_filtered_2 <- GenerationMetaData %>%
  mutate(Ear_Length_mm = ifelse(Ear_Length_mm < 8, NA, Ear_Length_mm))

# save EL x BW residuals
residsELBW <- lm(Ear_Length_mm ~ Body_Weight_g, data = Generation_filtered_2,
                 na.action = na.exclude)
Generation_filtered_2$Resids_ELBW <- resid(residsELBW)





##############################################################
# Get values from statistical analyses
##############################################################

# Refer to 'code/model_Generations.R' for model comparisons and statistical analyses


# > car::Anova(lm(rank(Body_Weight_g) ~ Sex * Population * Generation, data = GenerationMetaData), type = "III")
# Anova Table (Type III tests)
# 
# Response: rank(Body_Weight_g)
# Sum Sq  Df  F value    Pr(>F)    
# (Intercept)               1715176   1 201.0258 < 2.2e-16 ***
# Sex                        105397   1  12.3530  0.000486 ***
# Population                 470654   1  55.1626 5.829e-13 ***
# Generation                  41046   2   2.4054  0.091422 .  
# Sex:Population              33427   1   3.9177  0.048404 *  
# Sex:Generation              38224   2   2.2400  0.107672    
# Population:Generation       94670   2   5.5479  0.004174 ** 
# Sex:Population:Generation   46731   2   2.7386  0.065768 .  
# Residuals                 3745600 439                       
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

BW_deets <- ("&#42;&#42;pop x gen<br>\
                    &#42;sex x pop<br>\
              &#42;&#42;&#42;sex<br>\
              &#42;&#42;&#42;pop")


# > car::Anova(lm(Tail_Length_mm ~ Body_Weight_g + Sex * Population * Generation, data = Generation_filtered), type = "III")
# Anova Table (Type III tests)
# 
# Response: Tail_Length_mm
# Sum Sq  Df   F value    Pr(>F)    
# (Intercept)                31982   1 1448.6627 < 2.2e-16 ***
# Body_Weight_g                868   1   39.3297 8.922e-10 ***
# Sex                           88   1    3.9841  0.046577 *  
# Population                    43   1    1.9308  0.165406    
# Generation                   205   2    4.6466  0.010094 *  
# Sex:Population                90   1    4.0670  0.044367 *  
# Sex:Generation               247   2    5.6031  0.003968 ** 
# Population:Generation         76   2    1.7312  0.178340    
# Sex:Population:Generation    104   2    2.3548  0.096171 .  
# Residuals                   9250 419                        
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

TL_deets <- ("&#42;pop x sex<br>\
            &#42;&#42;gen x sex<br>\
            &#42;gen")


# > car::Anova(lm(rank(Ear_Length_mm) ~ Body_Weight_g + Sex * Population * Generation, data = Generation_filtered_2), type = "III")
# Anova Table (Type III tests)
# 
# Response: rank(Ear_Length_mm)
# Sum Sq  Df F value    Pr(>F)    
# (Intercept)                 49527   1  3.9040   0.04881 *  
# Body_Weight_g              310068   1 24.4414 1.102e-06 ***
# Sex                          8239   1  0.6494   0.42077    
# Population                    226   1  0.0178   0.89394    
# Generation                  78562   2  3.0964   0.04623 *  
# Sex:Population                179   1  0.0141   0.90553    
# Sex:Generation              21361   2  0.8419   0.43160    
# Population:Generation       23374   2  0.9212   0.39881    
# Sex:Population:Generation    2063   2  0.0813   0.92191    
# Residuals                 5429675 428                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

EL_deets <- ("&#42;gen")





##############################################################
# Plot sex-specific body mass ridgeplot across generations
##############################################################

MaleBW <- GenerationMetaData %>%
  filter(Sex == "Male")

FemaleBW <- GenerationMetaData %>%
  filter(Sex == "Female")


Fmassridge <- 
  ggplot(data = FemaleBW, aes(y = Generation, x = Body_Weight_g)) +
  geom_density_ridges(aes(point_color = Population, point_fill = Population, fill = Population),
                      jittered_points = TRUE, position = "raincloud",
                      alpha = 0.8, point_size = 1.85, point_alpha = 0.9, scale = 0.3,
                      point_shape=21, point_color = "white", color = NA) +
  geom_boxplot(aes(x = Body_Weight_g, y = Generation, color = Population),
               position = position_dodge(width = 0.01),  outlier.shape = NA,
               alpha = 0.5, width = 0.17) +
  scale_x_continuous(breaks = seq(from=5, to=45, by=10),
                     labels = seq(from=5, to=45, by=10),
                     limits = c(5,45)) +
  scale_discrete_manual("point_fill", values = c("dodgerblue4","goldenrod1"), guide="none") +
  scale_discrete_manual("point_color", values = c("dodgerblue4","goldenrod1"), guide = "none") +
  scale_color_manual(values=c("dodgerblue4","goldenrod1")) +
  scale_fill_manual(values=c("dodgerblue4","goldenrod1", "dodgerblue4","goldenrod1")) +
  guides(color=guide_legend(override.aes=list(fill=NA)),
         linetype=guide_legend(override.aes = list(fill=NA))) +# removes gray shading from legend
  coord_flip() +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.y = element_text(margin = margin(r = 5), size = 10, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.position = c(0.025, 0.94),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(0.35, "cm"),
        legend.text = element_text(size=7, family = "Palatino"),
        legend.title = element_blank(),
        plot.tag = element_text(family = "Palatino", size = 10, face = "italic", hjust = 1),
        plot.tag.position = c(0.15,1.025),
        plot.title = element_text(size = 8, face = "italic", hjust = 0.05, vjust = -3, family = "Palatino"),
        plot.title.position = "panel",
        plot.margin = unit(c(0, -0.25, 0, 0.45), "cm")) +
  labs(x = "Body Mass (g)",
       y = NULL,
       title = "Females")


Mmassridge <- 
  ggplot(data = MaleBW, aes(y = Generation, x = Body_Weight_g)) +
  geom_density_ridges(aes(point_color = Population, point_fill = Population, fill = Population),
                      jittered_points = TRUE, position = "raincloud",
                      alpha = 0.8, point_size = 1.85, point_alpha = 0.9, scale = 0.3,
                      point_shape=21, point_color = "white", color = NA) +
  geom_boxplot(aes(x = Body_Weight_g, y = Generation, color = Population),
               position = position_dodge(width = 0.01),  outlier.shape = NA,
               alpha = 0.5, width = 0.17) +
  scale_x_continuous(breaks = seq(from=5, to=45, by=10),
                     labels = seq(from=5, to=45, by=10),
                     limits = c(5,45)) +
  scale_discrete_manual("point_fill", values = c("dodgerblue4","goldenrod1"), guide="none") +
  scale_discrete_manual("point_color", values = c("dodgerblue4","goldenrod1"), guide = "none") +
  scale_color_manual(values=c("dodgerblue4","goldenrod1")) +
  scale_fill_manual(values=c("dodgerblue4","goldenrod1", "dodgerblue4","goldenrod1")) +
  guides(color=guide_legend(override.aes=list(fill=NA)),
         linetype=guide_legend(override.aes = list(fill=NA))) +
  coord_flip() +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = 'none',
        plot.tag = element_markdown(family = "Palatino", size = 7, face = "italic", hjust = 1),
        plot.tag.position = c(0.98,0.82),
        plot.title = element_text(size = 8, face = "italic", hjust = 0.05, vjust = -3, family = "Palatino"),
        plot.title.position = "panel",
        plot.margin = unit(c(0, 0.75, 0.5, 0), "cm")) + # top, right, bottom, lef
  labs(x = NULL,
       y = NULL,
       title = "Males",
       tag = BW_deets)


BW <- cowplot::plot_grid(Fmassridge, Mmassridge, labels = c('A)', ''), align = "h",
                         label_fontfamily = "Palatino", label_size = 12, label_x = 0.05, hjust = 0)





##############################################################
# Plot sex-specific tail length ridgeplot across generaitons
##############################################################

MaleTL <- Generation_filtered %>%
  filter(Sex == "Male")

FemaleTL <- Generation_filtered %>%
  filter(Sex == "Female")


Ftailridge <- 
  ggplot(data = FemaleTL, aes(y = Generation, x = Resids_TLBW)) +
  geom_density_ridges(aes(point_color = Population, point_fill = Population, fill = Population),
                      jittered_points = TRUE, position = "raincloud",
                      alpha = 0.8, point_size = 1.85, point_alpha = 0.9, scale = 0.3,
                      point_shape=21, point_color = "white", color = NA) +
  geom_boxplot(aes(x = Resids_TLBW, y = Generation, color = Population),
               position = position_dodge(width = 0.01),  outlier.shape = NA,
               alpha = 0.5, width = 0.17) +
  scale_x_continuous(breaks = seq(from=-20, to=20, by=10),
                     labels = seq(from=-20, to=20, by=10),
                     limits = c(-20,20)) +
  scale_discrete_manual("point_fill", values = c("dodgerblue4","goldenrod1"), guide="none") +
  scale_discrete_manual("point_color", values = c("dodgerblue4","goldenrod1"), guide = "none") +
  scale_color_manual(values=c("dodgerblue4","goldenrod1")) +
  scale_fill_manual(values=c("dodgerblue4","goldenrod1", "dodgerblue4","goldenrod1")) +
  guides(color=guide_legend(override.aes=list(fill=NA)),
         linetype=guide_legend(override.aes = list(fill=NA))) +# removes gray shading from legend
  coord_flip() +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 10), size = 10, face = "bold", family = "Palatino", hjust = 1.15),
        axis.title.y = element_text(margin = margin(r = 3), size = 9, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.position = "none",
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.5, "cm"),
        legend.text = element_text(size=8, family = "Palatino"),
        legend.title = element_blank(),
        plot.tag = element_text(family = "Palatino", size = 10, face = "italic"),
        plot.tag.position = c(0.17,1.025),
        plot.title = element_text(size = 8, face = "italic", hjust = 0.05, vjust = -3, family = "Palatino"),
        plot.title.position = "panel",
        plot.margin = unit(c(0, -0.25, 0, 0.375), "cm")) +
  labs(x = "Tail Length (resid.)",
       y = NULL,
       title = "Females")


Mtailridge <- 
  ggplot(data = MaleTL, aes(y = Generation, x = Resids_TLBW)) +
  geom_density_ridges(aes(point_color = Population, point_fill = Population, fill = Population),
                      jittered_points = TRUE, position = "raincloud",
                      alpha = 0.8, point_size = 1.85, point_alpha = 0.9, scale = 0.3,
                      point_shape=21, point_color = "white", color = NA) +
  geom_boxplot(aes(x = Resids_TLBW, y = Generation, color = Population),
               position = position_dodge(width = 0.01),  outlier.shape = NA,
               alpha = 0.5, width = 0.17, fill = "white") +
  scale_x_continuous(breaks = seq(from=-20, to=20, by=10),
                     labels = seq(from=-20, to=20, by=10),
                     limits = c(-20,20)) +
  scale_discrete_manual("point_fill", values = c("dodgerblue4","goldenrod1"), guide="none") +
  scale_discrete_manual("point_color", values = c("dodgerblue4","goldenrod1"), guide = "none") +
  scale_color_manual(values=c("dodgerblue4","goldenrod1")) +
  scale_fill_manual(values=c("dodgerblue4","goldenrod1", "dodgerblue4","goldenrod1")) +
  guides(color=guide_legend(override.aes=list(fill=NA)),
         linetype=guide_legend(override.aes = list(fill=NA))) +
  coord_flip() +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 10), size = 10, face = "bold", family = "Palatino", hjust = 0.6),
        axis.title.y = element_text(margin = margin(r = 10), size = 10, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = 'none',
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.5, "cm"),
        legend.text = element_text(size=8, family = "Palatino"),
        legend.title = element_blank(),
        plot.tag = element_markdown(family = "Palatino", size = 7, face = "italic", hjust = 1),
        plot.tag.position = c(0.98,0.83),
        plot.title = element_text(size = 8, face = "italic", hjust = 0.05, vjust = -3, family = "Palatino"),
        plot.title.position = "panel",
        plot.margin = unit(c(0, 0.75, 0.5, 0), "cm")) + # top, right, bottom, left (0.5, 0.5, 0.1, -0.5)
  labs(x = NULL,
       y = NULL,
       title = "Males",
       tag = TL_deets)


Tails <- cowplot::plot_grid(Ftailridge, Mtailridge, labels = c('B)', ''), align = "h",
                            label_fontfamily = "Palatino", label_size = 12, label_x = 0.05, hjust = 0)





##############################################################
# Plot sex-specific ear length ridgeplot across generaitons
##############################################################

MaleEL <- Generation_filtered_2 %>%
  filter(Sex == "Male")

FemaleEL <- Generation_filtered_2 %>%
  filter(Sex == "Female")


Fearridge <- 
  ggplot(data = FemaleEL, aes(y = Generation, x = Resids_ELBW)) +
  geom_density_ridges(aes(point_color = Population, point_fill = Population, fill = Population),
                      jittered_points = TRUE, position = "raincloud",
                      alpha = 0.8, point_size = 1.85, point_alpha = 0.9, scale = 0.3,
                      point_shape=21, point_color = "white", color = NA) +
  geom_boxplot(aes(x = Resids_ELBW, y = Generation, color = Population),
               position = position_dodge(width = 0.01),  outlier.shape = NA,
               alpha = 0.5, width = 0.17) +
  scale_x_continuous(breaks = seq(from=-5, to=10, by=5),
                     labels = seq(from=-5, to=10, by=5),
                     limits = c(-5,7)) +
  scale_discrete_manual("point_fill", values = c("dodgerblue4","goldenrod1"), guide="none") +
  scale_discrete_manual("point_color", values = c("dodgerblue4","goldenrod1"), guide = "none") +
  scale_color_manual(values=c("dodgerblue4","goldenrod1")) +
  scale_fill_manual(values=c("dodgerblue4","goldenrod1", "dodgerblue4","goldenrod1")) +
  guides(color=guide_legend(override.aes=list(fill=NA)),
         linetype=guide_legend(override.aes = list(fill=NA))) + # removes gray shading from legend
  coord_flip() +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 12), size = 10, face = "bold", family = "Palatino", hjust = 1.2),
        axis.title.y = element_text(margin = margin(r = 3), size = 9, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.position = "none",
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.5, "cm"),
        legend.text = element_text(size=8, family = "Palatino"),
        legend.title = element_blank(),
        plot.tag = element_text(family = "Palatino", size = 10, face = "italic"),
        plot.tag.position = c(0.17,1.025),
        plot.title = element_text(size = 8, face = "italic", hjust = 0.05, vjust = -3, family = "Palatino"),
        plot.title.position = "panel",
        plot.margin = unit(c(-0.1, -0.25, -.250, 0.5), "cm")) +
  labs(x = "Ear Length (resid.)",
       y = "Generation",
       title = "Females")


Mearridge <- 
  ggplot(data = MaleEL, aes(y = Generation, x = Resids_ELBW)) +
  geom_density_ridges(aes(point_color = Population, point_fill = Population, fill = Population),
                      jittered_points = TRUE, position = "raincloud",
                      alpha = 0.8, point_size = 1.85, point_alpha = 0.9, scale = 0.3,
                      point_shape=21, point_color = "white", color = NA) +
  geom_boxplot(aes(x = Resids_ELBW, y = Generation, color = Population),
               position = position_dodge(width = 0.01),  outlier.shape = NA,
               alpha = 0.5, width = 0.17, fill = "white") +
  scale_x_continuous(breaks = seq(from=-5, to=10, by=5),
                     labels = seq(from=-5, to=10, by=5),
                     limits = c(-5,7)) +
  scale_discrete_manual("point_fill", values = c("dodgerblue4","goldenrod1"), guide="none") +
  scale_discrete_manual("point_color", values = c("dodgerblue4","goldenrod1"), guide = "none") +
  scale_color_manual(values=c("dodgerblue4","goldenrod1")) +
  scale_fill_manual(values=c("dodgerblue4","goldenrod1", "dodgerblue4","goldenrod1")) +
  guides(color=guide_legend(override.aes=list(fill=NA)),
         linetype=guide_legend(override.aes = list(fill=NA))) +
  coord_flip() +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 10), size = 10, face = "bold", family = "Palatino", hjust = 0.6),
        axis.title.y = element_text(margin = margin(r = 10), size = 9, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = 'none',
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.5, "cm"),
        legend.text = element_text(size=8, family = "Palatino"),
        legend.title = element_blank(),
        plot.tag = element_markdown(family = "Palatino", size = 7, face = "italic", hjust = 1),
        plot.tag.position = c(0.98,0.86),
        plot.title = element_text(size = 8, face = "italic", hjust = 0.05, vjust = -3, family = "Palatino"),
        plot.title.position = "panel",
        plot.margin = unit(c(-0.1, 0.75, -0.25, 0), "cm")) + # top, right, bottom, left (0.5, 0.5, 0.1, -0.5)
  labs(x = NULL,
       y = NULL,
       title = "Males",
       tag = EL_deets)


Ears <- cowplot::plot_grid(Fearridge, Mearridge, labels = c('C)', ''), align = 'h',
                           label_fontfamily = "Palatino", label_size = 12, label_x = 0.05, hjust = 0)



cowplot::plot_grid(BW, Tails, Ears, ncol=1, nrow=3, label_fontfamily = "Palatino",
                   label_size = 12, label_x = 0.05, hjust = 0, align = 'hv')

ggsave("results/figures/Generations_colony.tiff", height = 7, width = 6, compression = "lzw")
ggsave("results/figures/Generations_colony.pdf", height = 7, width = 6)
