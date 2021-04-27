#!/usr/bin/env Rscript --vanilla

##############################################################
# Author: Mallory A. Ballinger
# Script first created: 12-Feb-2021
# Script last updated:  20-Apr-2021


# This script plots reaction norms of body mass and extremity lengths of New York
# and Brazil mice across environments (i.e. common garden experiment #2.)
# This script generates Fig. 5, Fig. 6, and Fig. S3 in Ballinger_et_al_2021_AmNat.


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
# Calculate residuals for plotting
##############################################################

# save TL x BW residuals
residsTLBW <- lm(Final_Tail_Length_mm ~ Body_Weight_g, data = PostDissectionMetaData,
                 na.action = na.exclude)
PostDissectionMetaData$Resids_TLBW <- resid(residsTLBW)


# Based on outlier tests (see 'code/model_RXNs.R'), any ear length
# greater than 17mm is an extreme outlier

PostDissection_filtered <- PostDissectionMetaData %>%
  mutate(Ear_Length_mm = ifelse(Ear_Length_mm > 17, NA, Ear_Length_mm))

# save EL x BW residuals
residsELBW <- lm(Ear_Length_mm ~ Body_Weight_g, data = PostDissection_filtered,
                 na.action = na.exclude)
PostDissection_filtered$Resids_ELBW <- resid(residsELBW)





##############################################################
# Get values from statistical analyses
##############################################################

# Refer to 'code/model_RXNs.R' for model comparisons and statistical analyses


# > car::Anova(lmer(Body_Weight_g ~ Sex * Population * Environment + (1|Line),
#                   +                 data = PostDissectionMetaData), type = "III")
# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: Body_Weight_g
# Chisq Df Pr(>Chisq)    
# (Intercept)                206.8937  1  < 2.2e-16 ***
# Sex                         42.1478  1  8.463e-11 ***
# Population                   4.0046  1    0.04538 *  
# Environment                  1.7226  1    0.18935    
# Sex:Population               2.5469  1    0.11051    
# Sex:Environment              0.0958  1    0.75694    
# Population:Environment       0.0927  1    0.76072    
# Sex:Population:Environment   0.5565  1    0.45568    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

BW_deets <- ("&#42;sex<br>\
              &#42;pop")



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



# > car::Anova(lmer(Final_Tail_Length_mm ~ Body_Weight_g + Sex * Population * Environment + (1|Line),
#                   +                 data = PostDissectionMetaData), type = "III")
# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: Final_Tail_Length_mm
# Chisq Df Pr(>Chisq)    
# (Intercept)                365.2417  1  < 2.2e-16 ***
# Body_Weight_g               51.9483  1  5.698e-13 ***
# Sex                          2.8003  1  0.0942464 .  
# Population                  11.7068  1  0.0006227 ***
# Environment                 86.0543  1  < 2.2e-16 ***
# Sex:Population               0.9737  1  0.3237585    
# Sex:Environment              0.3154  1  0.5744118    
# Population:Environment      14.8606  1  0.0001158 ***
# Sex:Population:Environment   0.0293  1  0.8640034 

TL_deets <- ("&#42;pop x env<br>\
              &#42;pop<br>\
              &#42;env")



# > car::Anova(lmer(Ear_Length_mm ~ Body_Weight_g + Sex * Population * Environment + (1|Line), data = PostDissection_filtered), type = "III")
# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: Ear_Length_mm
# Chisq Df Pr(>Chisq)    
# (Intercept)                410.0317  1  < 2.2e-16 ***
# Body_Weight_g               13.3652  1  0.0002563 ***
# Sex                          3.8759  1  0.0489827   
# Population                   0.5953  1  0.4403627    
# Environment                 57.6587  1  3.118e-14 ***
# Sex:Population               0.2132  1  0.6442756    
# Sex:Environment              1.2889  1  0.2562444    
# Population:Environment       0.4486  1  0.5030115    
# Sex:Population:Environment   0.2545  1  0.6139012   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

EL_deets <- glue("&#42;env")






##############################################################
# Plot sex-specific body mass RXN plot
##############################################################

# Sex-specific datasets
MaleData <- PostDissectionMetaData %>%
  filter(Sex == "Male")

FemaleData <- PostDissectionMetaData %>%
  filter(Sex == "Female")


BWrxnF <-
  ggplot(data=FemaleData, aes(x=Environment, y=Body_Weight_g)) +
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
  scale_y_continuous(breaks = seq(from=8, to=24, by=4), labels = seq(from=8, to=24, by=4), limits = c(8,25)) +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10), size = 11, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 10, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.position = c(0.047, 0.93),
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
       y = "Body Mass (g)",
       title = "Females")


BWrxnM <-
  ggplot(data=MaleData, aes(x=Environment, y=Body_Weight_g)) +
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
  scale_y_continuous(breaks = seq(from=8, to=24, by=4), labels = seq(from=8, to=24, by=4), limits = c(8,25)) +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10), size = 10, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 10, color = "black", family = "Palatino"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "none",
        plot.tag = element_markdown(family = "Palatino", size = 8, face = "italic", hjust = 1),
        plot.tag.position = c(0.96,0.88),
        plot.title = element_text(size = 8, face = "italic", hjust = 0.05, vjust = -3, family = "Palatino"),
        plot.title.position = "panel",
        plot.margin = unit(c(0.5, 0.5, 0.5, -0.42), "cm")) + # top, right, bottom, left (0.5, 0.5, 0.1, -0.5)
  labs(x = "",
       y = "",
       title = "Males",
       tag = BW_deets)


BW <- cowplot::plot_grid(BWrxnF, BWrxnM, align = "h", label_fontfamily = "Palatino",
                         label_size = 12, label_x = 0.05, hjust = 0)


ggsave("results/figures/RXNs_BW.tiff", height = 3.5, width = 6, compression = "lzw")
ggsave("results/figures/RXNs_BW.pdf", height = 3.5, width = 6)





##############################################################
# Plot rxn norm plots (tail length & ear length)
##############################################################

TLrxn <-
  ggplot(data=PostDissectionMetaData, aes(x=Environment, y=Resids_TLBW)) +
  geom_boxplot(aes(fill = Population), position = position_dodge(width=0.25), alpha=0.8, size = 0.5, outlier.shape = NA, notch = FALSE, coef=0) +
  stat_summary(
    fun = median,
    geom = 'line',
    aes(group = Population, color = Population),
    position = position_dodge(width = 0.25) , size=1.5, alpha = 1.2, linetype = "dashed") +
  geom_point(aes(fill = Population), position = position_jitterdodge(jitter.width = 0.15, dodge.width=0.25), alpha=5, size=2.25, shape=21, color = "white") +
  scale_color_manual(values=rep(c("dodgerblue4", "goldenrod1"))) +
  scale_fill_manual(values=rep(c("dodgerblue4", "goldenrod1"))) +
  scale_y_continuous(breaks = seq(from=-10, to=10, by=5), labels = seq(from=-10, to=10, by=5), limits = c(-10,10)) +
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
        plot.tag.position = c(0.955,0.93),
        plot.title = element_markdown(family = "Palatino"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  labs(x = "",
       y = "Tail Length (resid.)",
       tag = TL_deets)


ELrxn <-
  ggplot(data=PostDissection_filtered, aes(x=Environment, y=Resids_ELBW)) +
  geom_boxplot(aes(fill = Population), position = position_dodge(width=0.25), alpha=0.8, size = 0.5, outlier.shape = NA, notch = FALSE, coef=0) +
  stat_summary(
    fun = median,
    geom = 'line',
    aes(group = Population, color = Population),
    position = position_dodge(width = 0.25) , size=1.5, alpha = 1.2, linetype = "dashed") +
  geom_point(aes(fill = Population), position = position_jitterdodge(jitter.width = 0.15, dodge.width=0.25), alpha=5, size=2.25, shape=21, color = "white") +
  scale_color_manual(values=rep(c("dodgerblue4", "goldenrod1"))) +
  scale_fill_manual(values=rep(c("dodgerblue4", "goldenrod1"))) +
  scale_y_continuous(breaks = seq(from=-2, to=2, by=1), labels = seq(from=-2, to=2, by=1), limits = c(-2,2)) +
  theme_bw() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10), size = 12, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 12, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 11, color = "black", family = "Palatino"),
        legend.position = 'none',
        plot.tag = element_markdown(family = "Palatino", size = 10, face = "italic", hjust = 1),
        plot.tag.position = c(0.96,0.95),
        plot.title = element_markdown(family = "Palatino"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  labs(x = "",
       y = "Ear Length (resid.)",
       tag = EL_deets)


Extremities <- cowplot::plot_grid(TLrxn, ELrxn, ncol = 2, nrow = 1, labels = c('A)', 'B)'), label_fontfamily = "Palatino",
                   align = 'h', label_size = 14, label_x = 0.05, hjust = 0)

ggsave("results/figures/RXNs_Extremities.tiff", height = 4, width = 8.5, compression = "lzw")
ggsave("results/figures/RXNs_Extremities.pdf", height = 4, width = 8.5)





##############################################################
# Plot sex-specific BMI plot
##############################################################

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
        axis.title.y = element_markdown(margin = margin(r = 10), size = 11, face = "bold", family = "Palatino"),
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
        axis.title.y = element_text(margin = margin(r = 10), size = 10, face = "bold", family = "Palatino"),
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


ggsave("results/figures/RXNs_BMI.tiff", height = 3.5, width = 6, compression = "lzw")
ggsave("results/figures/RXNs_BMI.pdf", height = 3.5, width = 6)