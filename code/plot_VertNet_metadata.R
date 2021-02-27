#!/usr/bin/env Rscript --vanilla

##############################################################
# Author: Mallory A. Ballinger
# Script first created: 26-Feb-2021
# Script last updated:  26-Feb-2021


# This script plots body weight and tail length from all dowloaded entries
# of North and South American house mice from VertNet.
# This script generates Figure S1 in Ballinger_et_al_2021_AmNat


##############################################################
# Required packages
##############################################################

rm(list = ls()) # clear R's environment
library(tidyverse)
library(here)
library(cowplot)
library(ggtext)

##############################################################
# Import data
##############################################################

VertNetMetaData <- read_csv(here("data/processed/VertNet_Mus_processed.csv"),
                            col_types = cols(.default = col_character(),
                                             totallength_mm = col_double(),
                                             taillength_mm = col_double(),
                                             hindfootlength_mm = col_double(),
                                             earlength_mm = col_double(),
                                             bodyweight_g = col_double(),
                                             bodylength_mm = col_double(),
                                             year = col_double(),
                                             decimallatitude = col_double(),
                                             decimallongitude = col_double(),
                                             lengthinmm = col_double(),
                                             lengthunitsinferred = col_double(),
                                             massing = col_double(),
                                             massunitsinferred = col_double())) %>% # I first used guess_max = Inf to figure out what the column specifications are
  select(references, dynamicproperties, totallength_mm, taillength_mm, hindfootlength_mm, 
         earlength_mm, bodyweight_g, bodylength_mm, sex, lifestage, reproductivecondition,
         continent, country, stateprovince, decimallatitude, decimallongitude) %>%
  filter(continent == "North America" | continent == "South America") %>%
  filter(is.na(lifestage) | lifestage != "subadult") %>% # everything but subadult (including NAs)
  filter(is.na(lifestage) | lifestage != "undetermined") %>% # everything but undetermined (including NAs)
  filter(is.na(reproductivecondition) | reproductivecondition != "preg") %>% # everything but pregnant mice (including NAs)
  filter(sex == "female" | sex == "male") %>% # only males and females
  mutate(abslat = abs(decimallatitude),sex = as.factor(sex), lifestage = as.factor(lifestage)) %>%
  mutate_if(is.factor, fct_explicit_na, na_level = 'NA') %>% # let's you relevel NAs (I think)
  mutate(lifestage = fct_relevel(lifestage, "NA", "adult"))


##############################################################
# Data and Model testing
##############################################################
# is body weight normally distributed?
#hist(VertNetMetaData$taillength_mm, breaks = 50)
lmBW <- lm(bodyweight_g~abslat, data=VertNetMetaData)
# summary(lmBW)
# hist(resid(lmBW), breaks = 25)
# qqnorm(resid(lmBW))
# qqline(resid(lmBW))
# shapiro.test(resid(lmBW)) # not normally distributed - will have to use Spearman correlation

# is tail length normally distributed?
lmTLBW <- lm(taillength_mm~bodyweight_g, data=VertNetMetaData,
             na.action = na.exclude)
# #summary(lmTL)
# #plot(lmTL$residuals)
# hist(lmTLBW$residuals, breaks = 25)
# qqnorm(resid(lmTLBW))
# qqline(resid(lmTLBW))
# shapiro.test(resid(lmTLBW))

# Based on outlier tests (see 202101_VerNet_metadata.Rmd), any tail length less than 40 and greater than 120 are clear outliers
VertNetTail <- VertNetMetaData %>%
  filter(taillength_mm > 40, taillength_mm < 120)

lmTLBW.2 <- lm(taillength_mm~bodyweight_g, data = VertNetTail,
               na.action = na.exclude)
# hist(lmTLBW.2$residuals, breaks = 25)
# qqnorm(resid(lmTLBW.2))
# qqline(resid(lmTLBW.2))
# shapiro.test(resid(lmTLBW.2)) # still not normally distributed, but will use Spearman correlation

VertNetTail$Resids_TLBW <- resid(lmTLBW.2)

VertNetMetaData$Resids_TLBW <- resid(lmTLBW)



##############################################################
# Test for correlation between body weight and tail length
##############################################################
# # Bergmann's rule
# 
# # Pearson correlation since data are normally distributed
# corrBW <- cor.test(x = VertNetMetaData$abslat, y = VertNetMetaData$bodyweight_g, method = 'spearman')
# corrBW
# # rho = 0.031
# # p-value = 0.1786
# 
# 
# # R-squared value from linear model
# lmBW <- lm(bodyweight_g~abslat, data=VertNetMetaData)
# summary(lmBW)
# # adj R-squared = 0.0055
# 
# 
# # Allen's rule
# 
# #Spearman's correlation
# corrTL <- cor.test(x = VertNetMetaData$abslat, y = VertNetMetaData$Resids_TLBW, method = 'spearman') # not normally distributed, so need to use spearman
# corrTL
# # rho = -0.23
# # p < 2.2 x 10(-16)
# 
# # R-squared value from linear model
# lmTAIL <- lm(taillength_mm~bodyweight_g + abslat,
#              data = VertNetMetaData)
# summary(lmTAIL)
# # adj R-squared = 0.207
# # p < 2.2e-16






##############################################################
# Plot body weight and tail length vs. abs lat
##############################################################
# Bergmann's rule

Berg <-
  ggplot(data=VertNetMetaData, aes(x = abslat, y = bodyweight_g)) +
  geom_smooth(color = "gray", fill = "gray", linetype = "solid", method = "lm", se = FALSE) +
  geom_jitter(size = 2.2, height = 0, width = 0.2, alpha = 0.2) +
  scale_x_continuous(breaks = seq(from=0, to=65, by=10), labels = seq(from=0, to=65, by=10), limits = c(0,65)) +
  scale_y_continuous(breaks = seq(from=0, to=40, by=10), labels = seq(from=0, to=40, by=10), limits = c(0,40)) +
  theme_bw() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10), size = 11, face = "bold", family = "Palatino"),
        axis.title.y = element_text(margin = margin(r = 10), size = 11, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 9, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 9, color = "black", family = "Palatino"),
        legend.position = "none",
        plot.tag = element_markdown(family = "Palatino", size = 9, color = "black", hjust = 0), # need to use element_markdown since using ggtext
        plot.tag.position = c(0.2,0.93),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5, family = "Palatino")) +
  labs(x = "Degrees from the equator",
       y = "Body Mass (g)",
       tag = "R = 0.031  
       *p* = 0.18")


Allen <-
  ggplot(data=VertNetMetaData, aes(x = abslat, y = Resids_TLBW)) +
  geom_smooth(color = "gray", fill = "gray", linetype = "solid", method = "lm", se = FALSE) +
  geom_jitter(size = 2.2, height = 0, width = 0.2, alpha = 0.2) +
  scale_x_continuous(breaks = seq(from=0, to=65, by=10), labels = seq(from=0, to=65, by=10), limits = c(0,65)) +
  scale_y_continuous(breaks = seq(from=-60, to=60, by=10), labels = seq(from=-60, to=60, by=10), limits = c(-60,60)) +
  theme_bw() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10), size = 11, face = "bold", family = "Palatino"),
        axis.title.y = element_text(margin = margin(r = 8), size = 11, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 9, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 9, color = "black", family = "Palatino"),
        legend.position = "none",
        plot.tag = element_markdown(family = "Palatino", size = 9, color = "black", hjust = 0), # need to use element_markdown since using ggtext
        plot.tag.position = c(0.2,0.93),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  labs(x = "Degrees from the equator",
       y = "Tail Length Residuals",
       tag = "R = -0.23  
       *p* < 2.2e<sup>-16</sup>")


cowplot::plot_grid(Berg, Allen, labels = c('A', 'B'), ncol = 2, nrow = 1, label_fontfamily = "Palatino")

ggsave("figures/VertNet_metadata.tiff", height = 4, width = 9, compression = "lzw")
ggsave("figures/VertNet_metadata.pdf", height = 4, width = 9)

dev.off()