#!/usr/bin/env Rscript --vanilla

################################################################################

# Author: Mallory A. Ballinger

# This script models *relative* extremity length from colony house mice of the
# Brazil and New York populations (i.e. common garden experiment #1).
# Data were cleaned using the script ./clean_Generations.R.
# This script generates statistical analyses and Figure S1 for Ballinger_AmNat_2021.


################################################################################
# Required packages
################################################################################

rm(list = ls()) # clear R's environment
library(tidyverse)
library(here)
library(performance)
library(see)
library(lmerTest)
library(lme4)
library(emmeans)
library(car)
library(nlme)
library(Hmisc)
library(dotwhisker)
library(report)

set.seed(19910118)

#set global contrasts
options(contrasts = c("contr.sum", "contr.poly")) # for Type III SSS

################################################################################
# Import data               
################################################################################

GenerationMetaData <- read_csv(here("data/processed/GenerationColonyData.csv")) %>%
  select(-GUID, -Collector_ID, -Age_days, -Notes) %>% select(-1) %>%
  mutate(Sex = fct_relevel(Sex, "Male", "Female")) %>% # put males before females
  mutate(Population = fct_relevel(Population, "New York", "Brazil")) # put evolved cold pop first

# get sample size of each column
#colSums(!is.na(GenerationMetaData))





################################################################################
# Data and Model testing
# *Bergmann's rule*
################################################################################

# Check for and remove *extreme* outliers

#GenerationMetaData %>% ggplot(aes(x=Body_Weight_g)) + geom_histogram(binwidth = 1)

# BWOutliers <- GenerationMetaData %>%
#    summarise(GenerationMetaData, meanBW = mean(Body_Weight_g, na.rm = TRUE),
#             sdBW = sd(Body_Weight_g, na.rm = TRUE)) %>%
#    filter(Body_Weight_g < (meanBW - 3*sdBW) |
#           Body_Weight_g > (meanBW + 3*sdBW))

# no clear extreme outliers below 3 stdev from mean to remove


# Full Model
mod.full.BW <- lm(Body_Weight_g ~ Sex * Population * Generation,
                  data = GenerationMetaData)
#check_model(mod.full.BW)
#shapiro.test(resid(mod.full.BW)) # not normally distributed

mod.full_2.BW <- lm(rank(Body_Weight_g) ~ Sex * Population * Generation,
                    data = GenerationMetaData)
#check_model(mod.full_2.BW)
#shapiro.test(resid(mod.full_2.BW)) # still not normally distributed, but close and good enough

# Stats
summary(mod.full_2.BW)
#report(mod.full_2.BW)
# Kruskal-Wallis NP test
car::Anova(lm(rank(Body_Weight_g) ~ Sex * Population * Generation,
              data = GenerationMetaData), type = "III")
#car::Anova(mod.full_2.BW, type = "III")





################################################################################
# Data and Model testing
# *Allen's rule* - tail length
################################################################################

# Check for and remove *extreme* outliers (3 stdev from the mean)
#GenerationMetaData %>% ggplot(aes(x=Tail_Length_mm)) + geom_histogram(binwidth = 1)
#hist(GenerationMetaData$Tail_Length_mm, breaks = 100)

# TLOutliers <- GenerationMetaData %>%
#   summarise(GenerationMetaData, meanTL = mean(Tail_Length_mm, na.rm = TRUE),
#             sdTL = sd(Tail_Length_mm, na.rm = TRUE)) %>%
#   filter(Tail_Length_mm < (meanTL - 3*sdTL) |
#          Tail_Length_mm > (meanTL + 3*sdTL))

# n = 2 outliers below 50 mm (below 3 stdev from the mean)

Generation_filtered <- GenerationMetaData %>%
  mutate(Tail_Length_mm = ifelse(Tail_Length_mm < 50, NA, Tail_Length_mm)) %>%
  mutate(RelativeTail = (Tail_Length_mm) / (Body_Weight_g))

#Generation_filtered %>% ggplot(aes(x=Tail_Length_mm)) + geom_histogram(binwidth = 1)
#hist(Generation_filtered$Tail_Length_mm, breaks = 100)


# Full Model
mod.full.TL <- lm(RelativeTail ~ Sex * Population * Generation,
                  data = Generation_filtered)
#check_model(mod.full.TL)
#shapiro.test(resid(mod.full.TL)) # normally distributed

# Stats
summary(mod.full.TL)
report(mod.full.TL)
# One-way ANOVA P test
car::Anova(lm(RelativeTail ~ Sex * Population * Generation,
           data = Generation_filtered), type = "III")
#car::Anova(mod.full.TL, type = "III")

# Posthoc Tukey's test:
posthoc_TL <- emmeans(mod.full.TL, ~ Population * Generation * Sex)
pairs(posthoc_TL)
pwpp(posthoc_TL)
# Brazil has longer tails than NY across N1 and N2 generations





################################################################################
# Data and Model testing
# *Allen's rule* - ear length
################################################################################

# Check for and remove *extreme* outliers
#GenerationMetaData %>% ggplot(aes(x=Ear_Length_mm)) + geom_histogram(binwidth = 1)
#hist(GenerationMetaData$Ear_Length_mm, breaks = 100)

# ELOutliers <- GenerationMetaData %>%
#   summarise(GenerationMetaData, meanEL = mean(Ear_Length_mm, na.rm = TRUE),
#             sdEL = sd(Ear_Length_mm, na.rm = TRUE)) %>%
#   filter(Ear_Length_mm < (meanEL - 3*sdEL) |
#          Ear_Length_mm > (meanEL + 3*sdEL))

# n = 1 outliers below 8mm (below 3 stdev from mean)

Generation_filtered_2 <- GenerationMetaData %>%
  mutate(Ear_Length_mm = ifelse(Ear_Length_mm < 8, NA, Ear_Length_mm))  %>%
  mutate(RelativeEar = (Ear_Length_mm) / (Body_Weight_g))

#Generation_filtered_2 %>% ggplot(aes(x=Ear_Length_mm)) + geom_histogram(binwidth = 1)
#hist(Generation_filtered_2$Ear_Length_mm, breaks = 100)


# Full Model
mod.full.EL <- lm(RelativeEar ~ Sex * Population * Generation,
                  data = Generation_filtered_2)
#check_model(mod.full.EL)
#shapiro.test(resid(mod.full.EL)) # normally distributed


# Stats
summary(mod.full.EL)
car::Anova(lm(RelativeEar ~ Sex * Population * Generation,
              data = Generation_filtered_2), type = "III")





################################################################################
# Plot model results
################################################################################

Model_Generations <-
  dwplot(list(mod.full.BW, mod.full.TL, mod.full.EL), show_intercept = FALSE,
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) %>%
  relabel_predictors(Sex1 = "Sex",
                     Population1 = "Population",
                     Generation1 = "Generation (N1)",
                     Generation2 = "Generation (N2)",
                     'Sex1:Population1' = "Sex : Population",
                     'Sex1:Generation1' = "Sex : Generation (N1)",
                     'Sex1:Generation2' = "Sex : Generation (N2)",
                     'Population1:Generation1' = "Population : Generation (N1)",
                     'Population1:Generation2' = "Population : Generation (N2)",
                     'Sex1:Population1:Generation1' = "Sex : Population : Generation (N1)",
                     'Sex1:Population1:Generation2' = "Sex : Population : Generation (N2)",
                     '(Intercept)' = "Intercept") +
  scale_color_manual(values = c("purple", "black", "springgreen3"),
                     breaks = c("Model 1", "Model 2", "Model 3"),
                     labels = c("Body Mass", "Relative Tail Length", "Relative Ear Length")) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("(Trait) ~ Sex * Population * Generation") +
  theme(axis.title.x = element_text(margin = margin(t = 10), size = 10, face = "bold", family = "Palatino", hjust = 0.6),
        axis.title.y = element_text(margin = margin(r = 10), size = 10, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        plot.title = element_text(size = 9, face = "bold.italic", hjust = 0.5, vjust = 0, family = "Palatino"),
        legend.key.size = unit(0.35, "cm"),
        legend.position = c(0.735, 0.025),
        legend.text = element_text(size=8, family = "Palatino"),
        legend.justification = c(0, 0), 
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank(),
        legend.margin = margin(0.1, 3, 0.3, 0),
        legend.spacing.x = unit(0.5, "mm"),
        legend.spacing.y = unit(0.5, "mm"))


cowplot::plot_grid(Model_Generations, ncol=1, nrow=1, label_fontfamily = "Palatino", align = 'hv')

ggsave("results/figures/GenerationsModel_relative.tiff", height = 5, width = 7, compression = "lzw")
ggsave("results/figures/GenerationsModel_relative.pdf", height = 5, width = 7)

