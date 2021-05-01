#!/usr/bin/env Rscript --vanilla

################################################################################

# Author: Mallory A. Ballinger

# This script models body mass and extremity lengths from house mice of common garden
# experiment #2. Data were cleaned using the script ./clean_RXNs.R.
# This script generates statistical analyses for Ballinger_AmNat_2021, and generates Figure S2.


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
  mutate(Environment = fct_relevel(Environment, "Warm", "Cold" )) # put warm before cold (evolved before plasticity)


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

# Refer to exploratory/Modeling_RXN_2021-02-19.Rmd' for initial model testing and exploration.


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
#check_model(mod.full.BW)
#shapiro.test(resid(mod.full.BW)) # normally distributed


# Stats - all sexes
summary(mod.full.BW)
report(mod.full.BW)
# Anova P test
#anova(mod.full.BW) # Satterthwaite's method
car::Anova(mod.full.BW, type = "III")
# car::Anova(lmer(Body_Weight_g ~ Sex * Population * Environment + (1|Line),
# data = PostDissectionMetaData), type = "III")
# Posthoc Tukey's test
# post_BW <- emmeans::emmeans(mod.full.BW, specs = c("Population"), adjust = "tukey")
# pwpp(post_BW)


# # Full model - males
# mod.full.BW_M <- lmer(Body_Weight_g ~ Population * Environment + (1|Line),
#                    data = MaleData)
# #check_model(mod.full.BW_M)
# #shapiro.test(resid(mod.full.BW_M)) # normally distributed
# 
# 
# # Stats - males
# summary(mod.full.BW_M)
# # Anova P test
# car::Anova(mod.full.BW_M, type = "III")
# # Posthoc Tukey's test
# #emmeans::emmeans(mod.full.BW_M, ~ Population | Environment)
# 
# 
# # Full model - females
# mod.full.BW_F <- lmer(Body_Weight_g ~ Population * Environment + (1|Line),
#                       data = FemaleData)
# #check_model(mod.full.BW_F)
# #shapiro.test(resid(mod.full.BW_F)) # normally distributed
# 
# 
# # Stats - Females
# summary(mod.full.BW_F)
# # Anova P test
# car::Anova(mod.full.BW_F, type = "III")





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
# car::Anova(lmer(BMI_kg_m2 ~ Sex * Population * Environment + (1|Line),
#                 data = PostDissectionMetaData), type = "III")





################################################################################
# Data and Model testing
# *Allen's rule* - tail length
################################################################################

#PostDissectionMetaData %>% ggplot(aes(x=Final_Tail_Length_mm)) + geom_histogram(binwidth = 1)

# TLOutliers <- PostDissectionMetaData %>%
#    summarise(PostDissectionMetaData, meanTL = mean(Final_Tail_Length_mm, na.rm = TRUE),
#              sdTL = sd(Final_Tail_Length_mm, na.rm = TRUE)) %>%
#    filter(Final_Tail_Length_mm < (meanTL - 3*sdTL) |
#           Final_Tail_Length_mm > (meanTL + 3*sdTL))
 
# no outliers below 3 stdev from mean


# Full model - all sexes
mod.full.TL <- lmer(Final_Tail_Length_mm ~ Body_Weight_g + Sex * Population * Environment + (1|Line),
                    data = PostDissectionMetaData, REML = T)
#check_model(mod.full.TL)
#shapiro.test(resid(mod.full.TL)) # normally distributed


# Stats - all sexes
summary(mod.full.TL)
report(mod.full.TL)
# Anova P test
car::Anova(mod.full.TL, type = "III")
# car::Anova(lmer(Final_Tail_Length_mm ~ Body_Weight_g + Sex * Population * Environment + (1|Line),
# data = PostDissectionMetaData), type = "III")

posthoc_TL <- emmeans(mod.full.TL, ~ Population * Environment)
pairs(posthoc_TL)
pwpp(posthoc_TL)
# both have pop x env interactions





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
  mutate(Ear_Length_mm = ifelse(Ear_Length_mm > 17, NA, Ear_Length_mm))

# dataset for ear length > n = 78


# Full model - all sexes
mod.full.EL <- lmer(Ear_Length_mm ~ Body_Weight_g + Sex * Population * Environment + (1|Line),
                    data = PostDissection_filtered)

#check_model(mod.full.EL)
#shapiro.test(resid(mod.full.EL)) # normally distributed


# Stats - all sexes
summary(mod.full.EL)
report(mod.full.EL)
# Anova NP test
# car::Anova(lmer(Ear_Length_mm ~ Body_Weight_g + Sex * Population * Environment + (1|Line), data = PostDissection_filtered), type = "III")
car::Anova(mod.full.EL, type = "III")



# # Full model - males
# mod.full.EL_M <- lmer(Ear_Length_mm ~ Body_Weight_g + Population * Environment + (1|Line),
#                       data = MaleData)
# check_model(mod.full.EL_M)
# shapiro.test(resid(mod.full.EL_M)) # normally distributed
# 
# 
# # Stats - males
# summary(mod.full.EL_M)
# # Anova P test
# car::Anova(mod.full.EL_M, type = "III")
# # Posthoc Tukey's test
# # emmeans::emmeans(mod.full.BW_M, ~ Population | Environment)
# 
# 
# # Full model - females
# mod.full.EL_F <- lmer(Ear_Length_mm ~ Body_Weight_g + Population * Environment + (1|Line),
#                       data = FemaleData)
# check_model(mod.full.EL_F)
# shapiro.test(resid(mod.full.EL_F)) # not normally distributed
# 
# mod.full_2.EL_F <- lmer(rank(Ear_Length_mm) ~ Body_Weight_g + Population * Environment + (1|Line),
#                       data = FemaleData)
# check_model(mod.full_2.EL)
# shapiro.test(resid(mod.full_2.EL)) # normally distributed
# 
# # Stats - males
# summary(mod.full_2.EL_F)
# # Anova NP test
# car::Anova(lmer(rank(Ear_Length_mm) ~ Body_Weight_g + Population * Environment + (1|Line),
#                 data = FemaleData), type = "III")
# car::Anova(mod.full_2.EL_F, type = "III")





################################################################################
# Plot model results
################################################################################

Model_RXNs <-
  dwplot(list(mod.full.BW, mod.full.TL, mod.full.EL), show_intercept = FALSE,
         vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) %>%
  relabel_predictors(Body_Weight_g = "Body Mass (g)",
                     Sex1 = "Sex",
                     Population1 = "Population",
                     Environment1 = "Environment",
                     'Sex1:Population1' = "Sex : Population",
                     'Sex1:Environment1' = "Sex : Environment",
                     'Population1:Environment1' = "Population : Environment",
                     'Sex1:Population1:Environment1' = "Sex : Population : Environment",
                    '(Intercept)' = "Intercept") +
  scale_color_manual(values = c("purple", "black", "springgreen3"),
                     breaks = c("Model 1", "Model 2", "Model 3"),
                     labels = c("Body Mass", "Tail Length", "Ear Length")) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("(Trait) ~ Body Mass + Sex * Population * Environment") +
  theme(axis.title.x = element_text(margin = margin(t = 10), size = 10, face = "bold", family = "Palatino", hjust = 0.6),
        axis.title.y = element_text(margin = margin(r = 10), size = 10, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        plot.title = element_text(size = 9, face = "bold.italic", hjust = 0.5, vjust = 0, family = "Palatino"),
        legend.key.size = unit(0.35, "cm"),
        legend.position = c(0.835, 0.88),
        legend.text = element_text(size=8, family = "Palatino"),
        legend.justification = c(0, 0), 
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank(),
        legend.margin = margin(0.1, 3, 0.3, 0),
        legend.spacing.x = unit(0.5, "mm"),
        legend.spacing.y = unit(0.5, "mm"))


cowplot::plot_grid(Model_RXNs, ncol=1, nrow=1, label_fontfamily = "Palatino", align = 'hv')

ggsave("results/figures/RXNsModel.tiff", height = 5, width = 7, compression = "lzw")
ggsave("results/figures/RXNsModel.pdf", height = 5, width = 7)





################################################################################
# Plot model results - BMI
################################################################################

Model_BMI <-
  dwplot(list(mod.full.BMI), show_intercept = FALSE,
         vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) %>%
  relabel_predictors(Body_Weight_g = "Body Mass (g)",
                     Sex1 = "Sex",
                     Population1 = "Population",
                     Environment1 = "Environment",
                     'Sex1:Population1' = "Sex : Population",
                     'Sex1:Environment1' = "Sex : Environment",
                     'Population1:Environment1' = "Population : Environment",
                     'Sex1:Population1:Environment1' = "Sex : Population : Environment",
                     '(Intercept)' = "Intercept") +
  scale_color_manual(values = c("black"),
                     breaks = c("Model 1"),
                     labels = c("BMI")) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("BMI ~ Sex * Population * Environment") +
  theme(axis.title.x = element_text(margin = margin(t = 10), size = 10, face = "bold", family = "Palatino", hjust = 0.6),
        axis.title.y = element_text(margin = margin(r = 10), size = 10, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        plot.title = element_text(size = 9, face = "bold.italic", hjust = 0.5, vjust = 0, family = "Palatino"),
        legend.position = 'none')


cowplot::plot_grid(Model_BMI, ncol=1, nrow=1, label_fontfamily = "Palatino", align = 'hv')

ggsave("results/figures/RXNs_BMI_Model.tiff", height = 5, width = 7, compression = "lzw")
ggsave("results/figures/RXNs_BMI_Model.pdf", height = 5, width = 7)