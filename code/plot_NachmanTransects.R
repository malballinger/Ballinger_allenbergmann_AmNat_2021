#!/usr/bin/env Rscript --vanilla

##############################################################
# Author: Mallory A. Ballinger
# Script first created: 23-Feb-2021
# Script last updated:  25-Feb-2021


# This script plots body mass and tail length from wild-caught house mice,
# collected along a broad latitudinal cline, as part of the greater
# Nachman lab project on environmental adaptation in house mice.
# This script generates Figure 1 in Ballinger_et_al_2021_AmNat.


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

NachmanTransectsMetadata <- read_csv(here("data/raw/NachmanLab_MusTransects_Metadata_2021-02-15.csv")) %>%
  select(GUID, COLLECTORS, CONTINENT_OCEAN, COUNTRY, STATE_PROV, YEAR_COLLECTED, DEC_LAT, DEC_LONG, AGE_CLASS,
         EAR_FROM_NOTCH, HIND_FOOT_WITH_CLAW, REPRODUCTIVE_DATA_2, SEX, TAIL_LENGTH, TOTAL_LENGTH, WEIGHT) %>%
  filter(is.na(AGE_CLASS) | AGE_CLASS != "juvenile") %>% # everything but juvenile (including NAs)
  filter(is.na(AGE_CLASS) | AGE_CLASS != "subadult") %>% # everything but subadult (including NAs)
  filter(is.na(REPRODUCTIVE_DATA_2) | REPRODUCTIVE_DATA_2 != "pregnant") %>% # everything but pregnant mice (including NAs)
  mutate(ABS_LAT = abs(DEC_LAT), AGE_CLASS = as.factor(AGE_CLASS)) %>%
  mutate(BODY_LENGTH = TOTAL_LENGTH - TAIL_LENGTH) %>% # don't make BL as a factor or else the "NAs" will be treated as a factor (duh)
  #mutate(AGE_CLASS = fct_relevel(AGE_CLASS, "NA", "adult")) %>%
  mutate_if(is.factor, fct_explicit_na, na_level = 'NA') # let's you relevel NAs (I think)

# Sample sizes of full dataset (body mass)
FullSampleSize <- NachmanTransectsMetadata %>% summarize(N=n_distinct(GUID)) %>% pull(N)

##############################################################
# Data and Model testing
##############################################################
# is body weight normally distributed?
lmBW <- lm(WEIGHT~ABS_LAT, data=NachmanTransectsMetadata)
# summary(lmBW)
# hist(resid(lmBW), breaks = 25)
# qqnorm(resid(lmBW))
# qqline(resid(lmBW))
# shapiro.test(resid(lmBW)) # everything is normally distributed! Can move forward with body weight

# are there any tail length outliers?
hist(NachmanTransectsMetadata$TAIL_LENGTH, breaks = 25)
lmTL <- lm(TAIL_LENGTH~ABS_LAT, data=NachmanTransectsMetadata,
           na.action = na.exclude)
# summary(lmTL)
# hist(resid(lmTL), breaks = 25)
# qqnorm(resid(lmTL))
# qqline(resid(lmTL))
# clearly, any tail less than 40mm is an outlier (looks like there's one tail length that's < 20mm)
# will remove this outlier and then proceed

#NachmanTransectsMetadata$Resids_TLBW <- resid(lmTL)

Nachman_TailData <- NachmanTransectsMetadata %>%
  filter(TAIL_LENGTH > 20)

# Sample sizes for tail length
TailSampleSize <- Nachman_TailData %>% summarize(N=n_distinct(GUID)) %>% pull(N)

# is tail length normally distributed?
lmTLBW <- lm(TAIL_LENGTH~WEIGHT, data = Nachman_TailData)
# summary(lmTLBW)
# hist(resid(lmTLBW), breaks = 25)
# qqnorm(resid(lmTLBW))
# qqline(resid(lmTLBW))
# shapiro.test(resid(lmTLBW)) # not normally distributed, but much better after removal of outlier

# from exploratory analyses (see 'NachmanLab_transects.Rmd'), transforming the data doesn't make it any better
# will just have to use Spearman correlation instead of Pearson, since data are not normally distributed

# Save as residuals for model testing
Nachman_TailData$Resids_TLBW <- resid(lmTLBW) # saves residuals to Nachman_TailData dataset (the dataset used to create lm and residuals)

# Full model
mod.full.TLBW <- lm(TAIL_LENGTH~WEIGHT + ABS_LAT,
                    data = Nachman_TailData)
# summary(mod.full.TLBW)
# hist(resid(mod.full.TLBW), breaks = 25)
# plot(mod.full.TLBW) # looks good!
# qqnorm(resid(mod.full.TLBW))
# qqline(resid(mod.full.TLBW)) # puts line through it
# shapiro.test(resid(mod.full.TLBW)) # not normally distributed, but close

# from exploratory analyses, even log-transforming the data doesn't make it normally distributed
# so again, I'll just use Spearman correlation 



##############################################################
# Test for correlation between body weight and tail length
##############################################################
# # Bergmann's rule
# 
# # Pearson correlation since data are normally distributed
# corrBW <- cor.test(x = NachmanTransectsMetadata$ABS_LAT, y = NachmanTransectsMetadata$WEIGHT, method = 'pearson')
# corrBW
# # cor = 0.3439 (with a sign. p-value)
# 
# # Spearman correlation
# corrBW.2 <- cor.test(x = NachmanTransectsMetadata$ABS_LAT, y = NachmanTransectsMetadata$WEIGHT, method = 'spearman')
# corrBW.2
# # rho = 0.3405 (very sign. p-value)
# 
# # R-squared value from linear model
# lmBW <- lm(WEIGHT~ABS_LAT, data=NachmanTransectsMetadata)
# summary(lmBW)
# # adj R-squared = 0.1141
# 
# 
# # Allen's rule
# 
# #Spearman's correlation
# corrTL <- cor.test(x = NachmanTransectsMetadata$ABS_LAT, y = NachmanTransectsMetadata$Resids_TLBW, method = 'spearman') # not normally distributed, so need to use spearman
# corrTL
# # rho = -0.078
# # p = 0.26
# 
# # R-squared value from linear model
# lmTAIL <- lm(Resids_TLBW~ABS_LAT,
#              data = Nachman_TailData)
# summary(lmTAIL)
# # adj R-squared = -0.0048

# # R-squared value from linear model
# lmTAIL.2 <- lm(TAIL_LENGTH~WEIGHT + ABS_LAT,
#             data = Nachman_TailData)
# summary(lmTAIL.2)
# # adj R-squared = -0.0048

##############################################################
# Plot body weight and tail length vs. abs lat
##############################################################
# Bergmann's rule
Berg <-
  ggplot(data=NachmanTransectsMetadata, aes(x = ABS_LAT, y = WEIGHT)) +
  geom_smooth(color = "gray", fill = "gray", linetype = "solid", method = "lm", se = FALSE) +
  geom_jitter(size = 2.2, height = 0, width = 0.2, alpha = 0.7) +
  scale_x_continuous(breaks = seq(from=0, to=60, by=10), labels = seq(from=0, to=60, by=10), limits = c(0,55)) +
  scale_y_continuous(breaks = seq(from=0, to=30, by=10), labels = seq(from=0, to=30, by=10), limits = c(0,30)) +
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
  labs(x = "Degrees from the equator",
       y = "Body Mass (g)",
       tag = "R = 0.34  
       *p* < 0.0001")

Allen <-
  ggplot(data=Nachman_TailData, aes(x = ABS_LAT, y = Resids_TLBW)) +
  geom_smooth(color = "gray", fill = "gray", linetype = "solid", method = "lm", se = FALSE) +
  geom_jitter(size = 2.2, height = 0, width = 0.2, alpha = 0.7) +
  scale_x_continuous(breaks = seq(from=0, to=60, by=10), labels = seq(from=0, to=60, by=10), limits = c(0,55)) +
  scale_y_continuous(breaks = seq(from=-30, to=30, by=10), labels = seq(from=-30, to=30, by=10), limits = c(-30,30)) +
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
       tag = "R = -0.078  
       *p* = 0.26")

cowplot::plot_grid(Berg, Allen, labels = c('A', 'B'), ncol = 2, nrow = 1, label_fontfamily = "Palatino")


ggsave("figures/Nachman_transects.tiff", height = 4, width = 9, compression = "lzw")
ggsave("figures/Nachman_transects.pdf", height = 4, width = 9)

dev.off()

