#!/usr/bin/env Rscript --vanilla

################################################################################
# Author: Mallory A. Ballinger
# Script first created: 03-Apr-2021
# Script last updated:  07-Apr-2021


# This script models body mass and extremity length from colony house mice of the
# Brazil and New York populations (i.e. common garden experiment #1).
# Data were cleaned using the script ./clean_Generations.R.
# This script generates statistical analyses for Ballinger_et_al_2021_AmNat.


################################################################################
# Required packages
################################################################################

rm(list = ls()) # clear R's environment
library(tidyverse)
library(here)
library(performance)
library(see)
library(car)
library(nlme)
library(Hmisc)
library(dotwhisker)

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
# no clear extreme outliers for body weight


# Full Model
mod.full.BW <- lm(Body_Weight_g ~ Sex * Population * Generation,
                  data = GenerationMetaData)
#check_model(mod.full.BW)
#shapiro.test(resid(mod.full.BW)) # not normally distributed

mod.full_2.BW <- lm(rank(Body_Weight_g) ~ Sex * Population * Generation,
                    data = GenerationMetaData)
#check_model(mod.full_2.BW)
#shapiro.test(resid(mod.full_2.BW)) # still not normally distributed, but extremeley close and good enough

# Stats
summary(mod.full_2.BW)
# Kruskal-Wallis NP test
car::Anova(lm(rank(Body_Weight_g) ~ Sex * Population * Generation,
                    data = GenerationMetaData), type = "III")
#car::Anova(mod.full_2.BW, type = "III")





################################################################################
# Data and Model testing
# *Allen's rule* - tail length
################################################################################

# Check for and remove *extreme* outliers
#GenerationMetaData %>% ggplot(aes(x=Tail_Length_mm)) + geom_histogram(binwidth = 1)
#hist(GenerationMetaData$Tail_Length_mm, breaks = 100)
# from eyeball test, any tail shorter than 50mm is an extreme outlier

Generation_filtered <- GenerationMetaData %>%
  mutate(Tail_Length_mm = ifelse(Tail_Length_mm < 50, NA, Tail_Length_mm))

#Generation_filtered %>% ggplot(aes(x=Tail_Length_mm)) + geom_histogram(binwidth = 1)
#hist(Generation_filtered$Tail_Length_mm, breaks = 100)


# Full Model
mod.full.TL <- lm(Tail_Length_mm ~ Body_Weight_g + Sex * Population * Generation,
                  data = Generation_filtered)
#check_model(mod.full.TL)
#shapiro.test(resid(mod.full.TL)) # normally distributed

# Stats
summary(mod.full.TL)
# One-way ANOVA P test
car::Anova(lm(Tail_Length_mm ~ Body_Weight_g + Sex * Population * Generation,
           data = Generation_filtered), type = "III")
#car::Anova(mod.full.TL, type = "III")





################################################################################
# Data and Model testing
# *Allen's rule* - ear length
################################################################################

# Check for and remove *extreme* outliers
#GenerationMetaData %>% ggplot(aes(x=Ear_Length_mm)) + geom_histogram(binwidth = 1)
#hist(GenerationMetaData$Ear_Length_mm, breaks = 100)
# from eyeball test, any ear shorter than 8mm is an extreme outlier

Generation_filtered_2 <- GenerationMetaData %>%
  mutate(Ear_Length_mm = ifelse(Ear_Length_mm < 8, NA, Ear_Length_mm))

#Generation_filtered_2 %>% ggplot(aes(x=Ear_Length_mm)) + geom_histogram(binwidth = 1)
#hist(Generation_filtered_2$Ear_Length_mm, breaks = 100)


# Full Model
mod.full.EL <- lm(Ear_Length_mm ~ Body_Weight_g + Sex * Population * Generation,
                  data = Generation_filtered_2)
#check_model(mod.full.EL)
#shapiro.test(resid(mod.full.EL)) # not normally distributed

mod.full_2.EL <- lm(rank(Ear_Length_mm) ~ Body_Weight_g + Sex * Population * Generation,
                         data = Generation_filtered_2)
#check_model(mod.full_2.EL)
#shapiro.test(resid(mod.full_2.EL)) # still not normally distributed, but extremeley close and good enough


# Stats
summary(mod.full_2.EL)
# Kruskal-Wallis NP test
car::Anova(lm(rank(Ear_Length_mm) ~ Body_Weight_g + Sex * Population * Generation,
              data = Generation_filtered_2), type = "III")
#car::Anova(mod.full_2.EL, type = "III")





################################################################################
# Plot model results
################################################################################

Model_Generations <-
  dwplot(list(mod.full.BW, mod.full.TL, mod.full.EL), show_intercept = TRUE,
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) %>%
  relabel_predictors(Body_Weight_g = "Body Mass (g)",
                     SexFemale = "Sex (Female)",
                     PopulationBrazil = "Population (Brazil)",
                     GenerationN1 = "Generation (N1)",
                     GenerationN2 = "Generation (N2)",
                     'SexFemale:PopulationBrazil' = "Sex (Female) : Population (Brazil)",
                     'SexFemale:GenerationN1' = "Sex (Female) : Generation (N1)",
                     'SexFemale:GenerationN2' = "Sex (Female) : Generation (N2)",
                     'PopulationBrazil:GenerationN1' = "Population (Brazil) : Generation (N1)",
                     'PopulationBrazil:GenerationN2' = "Population (Brazil) : Generation (N2)",
                     'SexFemale:PopulationBrazil:GenerationN1' = "Sex (Female) : Population (Brazil) : Generation (N1)",
                     'SexFemale:PopulationBrazil:GenerationN2' = "Sex (Female) : Population (Brazil) : Generation (N2)",
                     '(Intercept)' = "Intercept (Male - New York - Generation N0)") +
  scale_color_manual(values = c("purple", "black", "springgreen3"),
                     breaks = c("Model 1", "Model 2", "Model 3"),
                     labels = c("Body Mass", "Tail Length", "Ear Length")) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("(Trait) ~ Body Weight + Sex * Population * Generation") +
  theme(axis.title.x = element_text(margin = margin(t = 10), size = 10, face = "bold", family = "Palatino", hjust = 0.6),
        axis.title.y = element_text(margin = margin(r = 10), size = 10, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        plot.title = element_text(size = 9, face = "bold.italic", hjust = 0.5, vjust = 0, family = "Palatino"),
        legend.key.size = unit(0.35, "cm"),
        legend.position = c(0.785, 0.88),
        legend.text = element_text(size=8, family = "Palatino"),
        legend.justification = c(0, 0), 
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank(),
        legend.margin = margin(0.1, 3, 0.3, 0),
        legend.spacing.x = unit(0.5, "mm"),
        legend.spacing.y = unit(0.5, "mm"))


cowplot::plot_grid(Model_Generations, ncol=1, nrow=1, label_fontfamily = "Palatino", align = 'hv')

ggsave("results/figures/GenerationsModel.tiff", height = 5, width = 7, compression = "lzw")
ggsave("results/figures/GenerationsModel.pdf", height = 5, width = 7)

