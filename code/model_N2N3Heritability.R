#!/usr/bin/env Rscript --vanilla

################################################################################

# Author: Mallory A. Ballinger

# This script calculates heritability of body weight, relative tail length, and
# relative ear length between N2 (parents) and N3 (offspring).
# Heritabilities are plotted in Figures 2 and S3 in Ballinger_AmNat_2021.


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
library(magrittr)
library(patchwork)

set.seed(19910118)


################################################################################
# Import data               
################################################################################

N2N3h2 <- read_csv(here("data/processed/N2N3_h2_data.csv"),
                       col_types = cols(Line_ID = col_character(),
                                        Mother_Line = col_character(),
                                        Father_Line = col_character(),
                                        Midparent_ID = col_character(),
                                        Notes = col_character())) %>%
  select(-GUID, -Hindfoot_Length_mm, -Body_Length_mm) %>%
  filter(is.na(Notes) | Notes != "pregnant")


################################################################################
# Data and Model testing
################################################################################

# See ./code/model_Generations_relative_N0-N4.R for identifying and removing outliers

# BW: there were no clear extreme outliers *below* 3 stdev from mean to remove

# TL: there were 2 outliers below 50 mm (below 3 stdev from the mean) to remove,
#     but these data are not present in this subset data

h2_filtered <- N2N3h2 %>%
  mutate(RelativeTail = (Tail_Length_mm) / (Body_Weight_g))

# EL: there was 1 outlier below 8mm (*below* 3 stdev from mean) to remove,
#     but this data point in not present in this subset data

h2_filtered <- h2_filtered %>%
  mutate(RelativeEar = (Ear_Length_mm) / (Body_Weight_g))


################################################################################
# Construct parental midpoint values for each trait (non-standardized values)
################################################################################

### Body Weight

# parents
parents <- h2_filtered %>%
  filter(Generation == "N2")

midparent_values <- parents %>%
  mutate(Parental_Midpoint_BW = case_when(Collector_ID == "FMM442" ~ # 261x239_B5xB6
                                            mean(c(Body_Weight_g[8], Body_Weight_g[23])),
                                      Collector_ID == "FMM421" ~ # 222x254_A1xA2
                                         mean(c(Body_Weight_g[4], Body_Weight_g[19])),
                                      Collector_ID == "FMM377" ~ # 221x215_B1xB2
                                         mean(c(Body_Weight_g[2], Body_Weight_g[21])),
                                      Collector_ID == "FMM440" ~ # 224x225_A1xA2
                                         mean(c(Body_Weight_g[7], Body_Weight_g[29])),
                                      Collector_ID == "FMM538" ~ # 260x220_A1xA2
                                         mean(c(Body_Weight_g[14], Body_Weight_g[28])),
                                      Collector_ID == "FMM542" ~ # 250x247_A1xA2
                                        mean(c(Body_Weight_g[15], Body_Weight_g[27])),
                                      Collector_ID == "FMM534" ~ # 250x247_A3xA2
                                        mean(c(Body_Weight_g[13], Body_Weight_g[27])),
                                      Collector_ID == "FMM443" ~ # 209x208_A1xA2
                                        mean(c(Body_Weight_g[9], Body_Weight_g[24])),
                                      Collector_ID == "FMM453" ~ # 222x254_A5xA6
                                        mean(c(Body_Weight_g[10], Body_Weight_g[26])),
                                      Collector_ID == "FMM406" ~ # 263x249_A1xA2
                                        mean(c(Body_Weight_g[3], Body_Weight_g[18])),
                                      Collector_ID == "FMM373" ~ # 254x215_B1xB2
                                        mean(c(Body_Weight_g[1], Body_Weight_g[17])),
                                      Collector_ID == "FMM458" ~ # 228x238_A1xA2
                                        mean(c(Body_Weight_g[11], Body_Weight_g[25])),
                                      Collector_ID == "FMM428" ~ # 228x238_B1xB2
                                        mean(c(Body_Weight_g[5], Body_Weight_g[20])),
                                      Collector_ID == "FMM437" ~ # 255x193_A1xA8
                                        mean(c(Body_Weight_g[6], Body_Weight_g[22])),
                                      ### New York Parents ###
                                      Collector_ID == "MJS314" ~ # 84x89_A1xA2
                                        mean(c(Body_Weight_g[42], Body_Weight_g[51])),
                                      Collector_ID == "MJS159" ~ # 66x67_B1xB2
                                        mean(c(Body_Weight_g[30], Body_Weight_g[45])),
                                      Collector_ID == "MJS328" ~ # 76x56_A1xA2
                                        mean(c(Body_Weight_g[43], Body_Weight_g[57])),
                                      Midparent_ID == "82x81_C11xC4" & Collector_ID == "MJS226" ~
                                        mean(c(Body_Weight_g[33], Body_Weight_g[46])),
                                      Collector_ID == "MJS228" ~ # 92x91_A3xA2
                                        mean(c(Body_Weight_g[34], Body_Weight_g[49])),
                                      Collector_ID == "MJS238" ~ # 83x80_A?xA8
                                        mean(c(Body_Weight_g[38], Body_Weight_g[53])),
                                      Collector_ID == "MJS243" ~ # 44x42_C5xC4
                                        mean(c(Body_Weight_g[41], Body_Weight_g[56])),
                                      Collector_ID == "MJS231" ~ # 34x38_C9xC4
                                        mean(c(Body_Weight_g[35], Body_Weight_g[50])),
                                      Collector_ID == "MJS236" ~ # 9x11_A3xA4
                                        mean(c(Body_Weight_g[37], Body_Weight_g[48])),
                                      Collector_ID == "MJS332" ~ # 19x13_B1xB4
                                        mean(c(Body_Weight_g[44], Body_Weight_g[52])),
                                      Midparent_ID == "9x11_C3xC2" & Collector_ID == "MJS225" ~
                                        mean(c(Body_Weight_g[32], Body_Weight_g[47])),
                                      Collector_ID == "MJS241" ~ # 34x38_B3xB4
                                        mean(c(Body_Weight_g[40], Body_Weight_g[55])),
                                      Collector_ID == "MJS239" ~ # 34x38_A1xA2
                                        mean(c(Body_Weight_g[39], Body_Weight_g[54]))))
                                                          

### Relative Tail Length

midparent_values <- midparent_values %>%
  mutate(Parental_Midpoint_TL = case_when(Collector_ID == "FMM442" ~ # 261x239_B5xB6
                                            mean(c(RelativeTail[8], RelativeTail[23])),
                                          Collector_ID == "FMM421" ~ # 222x254_A1xA2
                                            mean(c(RelativeTail[4], RelativeTail[19])),
                                          Collector_ID == "FMM377" ~ # 221x215_B1xB2
                                            mean(c(RelativeTail[2], RelativeTail[21])),
                                          Collector_ID == "FMM440" ~ # 224x225_A1xA2
                                            mean(c(RelativeTail[7], RelativeTail[29])),
                                          Collector_ID == "FMM538" ~ # 260x220_A1xA2
                                            mean(c(RelativeTail[14], RelativeTail[28])),
                                          Collector_ID == "FMM542" ~ # 250x247_A1xA2
                                            mean(c(RelativeTail[15], RelativeTail[27])),
                                          Collector_ID == "FMM534" ~ # 250x247_A3xA2
                                            mean(c(RelativeTail[13], RelativeTail[27])),
                                          Collector_ID == "FMM443" ~ # 209x208_A1xA2
                                            mean(c(RelativeTail[9], RelativeTail[24])),
                                          Collector_ID == "FMM453" ~ # 222x254_A5xA6
                                            mean(c(RelativeTail[10], RelativeTail[26])),
                                          Collector_ID == "FMM406" ~ # 263x249_A1xA2
                                            mean(c(RelativeTail[3], RelativeTail[18])),
                                          Collector_ID == "FMM373" ~ # 254x215_B1xB2
                                            mean(c(RelativeTail[1], RelativeTail[17])),
                                          Collector_ID == "FMM458" ~ # 228x238_A1xA2
                                            mean(c(RelativeTail[11], RelativeTail[25])),
                                          Collector_ID == "FMM428" ~ # 228x238_B1xB2
                                            mean(c(RelativeTail[5], RelativeTail[20])),
                                          Collector_ID == "FMM437" ~ # 255x193_A1xA8
                                            mean(c(RelativeTail[6], RelativeTail[22])),
                                          ### New York Parents ###
                                          Collector_ID == "MJS314" ~ # 84x89_A1xA2
                                            mean(c(RelativeTail[42], RelativeTail[51])),
                                          Collector_ID == "MJS159" ~ # 66x67_B1xB2
                                            mean(c(RelativeTail[30], RelativeTail[45])),
                                          Collector_ID == "MJS328" ~ # 76x56_A1xA2
                                            mean(c(RelativeTail[43], RelativeTail[57])),
                                          Midparent_ID == "82x81_C11xC4" & Collector_ID == "MJS226" ~
                                            mean(c(RelativeTail[33], RelativeTail[46])),
                                          Collector_ID == "MJS228" ~ # 92x91_A3xA2
                                            mean(c(RelativeTail[34], RelativeTail[49])),
                                          Collector_ID == "MJS238" ~ # 83x80_A?xA8
                                            mean(c(RelativeTail[38], RelativeTail[53])),
                                          Collector_ID == "MJS243" ~ # 44x42_C5xC4
                                            mean(c(RelativeTail[41], RelativeTail[56])),
                                          Collector_ID == "MJS231" ~ # 34x38_C9xC4
                                            mean(c(RelativeTail[35], RelativeTail[50])),
                                          Collector_ID == "MJS236" ~ # 9x11_A3xA4
                                            mean(c(RelativeTail[37], RelativeTail[48])),
                                          Collector_ID == "MJS332" ~ # 19x13_B1xB4
                                            mean(c(RelativeTail[44], RelativeTail[52])),
                                          Midparent_ID == "9x11_C3xC2" & Collector_ID == "MJS225" ~
                                            mean(c(RelativeTail[32], RelativeTail[47])),
                                          Collector_ID == "MJS241" ~ # 34x38_B3xB4
                                            mean(c(RelativeTail[40], RelativeTail[55])),
                                          Collector_ID == "MJS239" ~ # 34x38_A1xA2
                                            mean(c(RelativeTail[39], RelativeTail[54]))))


### Relative Ear Length

midparent_values <- midparent_values %>%
  mutate(Parental_Midpoint_EL = case_when(Collector_ID == "FMM442" ~ # 261x239_B5xB6
                                            mean(c(RelativeEar[8], RelativeEar[23])),
                                          Collector_ID == "FMM421" ~ # 222x254_A1xA2
                                            mean(c(RelativeEar[4], RelativeEar[19])),
                                          Collector_ID == "FMM377" ~ # 221x215_B1xB2
                                            mean(c(RelativeEar[2], RelativeEar[21])),
                                          Collector_ID == "FMM440" ~ # 224x225_A1xA2
                                            mean(c(RelativeEar[7], RelativeEar[29])),
                                          Collector_ID == "FMM538" ~ # 260x220_A1xA2
                                            mean(c(RelativeEar[14], RelativeEar[28])),
                                          Collector_ID == "FMM542" ~ # 250x247_A1xA2
                                            mean(c(RelativeEar[15], RelativeEar[27])),
                                          Collector_ID == "FMM534" ~ # 250x247_A3xA2
                                            mean(c(RelativeEar[13], RelativeEar[27])),
                                          Collector_ID == "FMM443" ~ # 209x208_A1xA2
                                            mean(c(RelativeEar[9], RelativeEar[24])),
                                          Collector_ID == "FMM453" ~ # 222x254_A5xA6
                                            mean(c(RelativeEar[10], RelativeEar[26])),
                                          Collector_ID == "FMM406" ~ # 263x249_A1xA2
                                            mean(c(RelativeEar[3], RelativeEar[18])),
                                          Collector_ID == "FMM373" ~ # 254x215_B1xB2
                                            mean(c(RelativeEar[1], RelativeEar[17])),
                                          Collector_ID == "FMM458" ~ # 228x238_A1xA2
                                            mean(c(RelativeEar[11], RelativeEar[25])),
                                          Collector_ID == "FMM428" ~ # 228x238_B1xB2
                                            mean(c(RelativeEar[5], RelativeEar[20])),
                                          Collector_ID == "FMM437" ~ # 255x193_A1xA8
                                            mean(c(RelativeEar[6], RelativeEar[22])),
                                          ### New York Parents ###
                                          Collector_ID == "MJS314" ~ # 84x89_A1xA2
                                            mean(c(RelativeEar[42], RelativeEar[51])),
                                          Collector_ID == "MJS159" ~ # 66x67_B1xB2
                                            mean(c(RelativeEar[30], RelativeEar[45])),
                                          Collector_ID == "MJS328" ~ # 76x56_A1xA2
                                            mean(c(RelativeEar[43], RelativeEar[57])),
                                          Midparent_ID == "82x81_C11xC4" & Collector_ID == "MJS226" ~
                                            mean(c(RelativeEar[33], RelativeEar[46])),
                                          Collector_ID == "MJS228" ~ # 92x91_A3xA2
                                            mean(c(RelativeEar[34], RelativeEar[49])),
                                          Collector_ID == "MJS238" ~ # 83x80_A?xA8
                                            mean(c(RelativeEar[38], RelativeEar[53])),
                                          Collector_ID == "MJS243" ~ # 44x42_C5xC4
                                            mean(c(RelativeEar[41], RelativeEar[56])),
                                          Collector_ID == "MJS231" ~ # 34x38_C9xC4
                                            mean(c(RelativeEar[35], RelativeEar[50])),
                                          Collector_ID == "MJS236" ~ # 9x11_A3xA4
                                            mean(c(RelativeEar[37], RelativeEar[48])),
                                          Collector_ID == "MJS332" ~ # 19x13_B1xB4
                                            mean(c(RelativeEar[44], RelativeEar[52])),
                                          Midparent_ID == "9x11_C3xC2" & Collector_ID == "MJS225" ~
                                            mean(c(RelativeEar[32], RelativeEar[47])),
                                          Collector_ID == "MJS241" ~ # 34x38_B3xB4
                                            mean(c(RelativeEar[40], RelativeEar[55])),
                                          Collector_ID == "MJS239" ~ # 34x38_A1xA2
                                            mean(c(RelativeEar[39], RelativeEar[54]))))


################################################################################
# Model heritability for each trait using midparent values
################################################################################

### Body Weight

# New York
NY_Parental_midpoint_BW <- midparent_values %>%
  filter(Population == "New York") %>%
  select(Population, Generation, Sex, Midparent_ID, Parental_Midpoint_BW)

NY_Offspring_BW <- h2_filtered %>%
  filter(Generation == "N3" & Population == "New York") %>%
  select(Population, Generation, Sex, Midparent_ID, Body_Weight_g) %>%
  rename("Offspring_BW" = "Body_Weight_g")

NY_parents_offspring_BW <- full_join(NY_Parental_midpoint_BW, NY_Offspring_BW, by = "Midparent_ID")

# ggplot(data = NY_parents_offspring_BW, aes(x=Parental_Midpoint_BW, y = Offspring_BW)) +
#   geom_point(na.rm = FALSE) +
#   geom_smooth(method = "lm")

# Model
lm.NY_parents_offspring_BW <- lm(Offspring_BW ~ Parental_Midpoint_BW, data = NY_parents_offspring_BW)
#check_model(lm.NY_parents_offspring_BW) # data look pretty normal
#car::Anova(lm.NY_parents_offspring_BW)


NY_parents_offspring_BW_h2 <- signif(as.double(summary(lm.NY_parents_offspring_BW)$coef[[2]]), digits = 2) # heritability
NY_parents_offspring_BW_se <- signif(as.double(summary(lm.NY_parents_offspring_BW)$coef[[4]]), digits = 2) # stderr
NY_parents_offspring_BW_pval <- signif(as.double(summary(lm.NY_parents_offspring_BW)$coef[[8]]), digits = 2) # p-value


# Brazil
BZ_Parental_midpoint_BW <- midparent_values %>%
  filter(Population == "Brazil") %>%
  select(Population, Generation, Midparent_ID, Parental_Midpoint_BW)

BZ_Offspring_BW <- h2_filtered %>%
  filter(Generation == "N3" & Population == "Brazil") %>%
  select(Population, Generation, Midparent_ID, Body_Weight_g) %>%
  rename("Offspring_BW" = "Body_Weight_g")

BZ_parents_offspring_BW <- full_join(BZ_Parental_midpoint_BW, BZ_Offspring_BW, by = "Midparent_ID")

# ggplot(data = BZ_parents_offspring_BW, aes(x=Parental_Midpoint_BW, y = Offspring_BW)) +
#   geom_point(na.rm = FALSE) +
#   geom_smooth(method = "lm")

# Model
lm.BZ_parents_offspring_BW <- lm(Offspring_BW ~ Parental_Midpoint_BW, data = BZ_parents_offspring_BW)
#check_model(lm.BZ_parents_offspring_BW) # looks great!
#car::Anova(lm.BZ_parents_offspring_BW)

BZ_parents_offspring_BW_h2 <- signif(as.double(summary(lm.BZ_parents_offspring_BW)$coef[[2]]), digits = 2) # heritability
BZ_parents_offspring_BW_se <- signif(as.double(summary(lm.BZ_parents_offspring_BW)$coef[[4]]), digits = 2) # stderr
BZ_parents_offspring_BW_pval <- signif(as.double(summary(lm.BZ_parents_offspring_BW)$coef[[8]]), digits = 2) # p-value


### Tail Length

# New York
NY_Parental_midpoint_TL <- midparent_values %>%
  filter(Population == "New York") %>%
  select(Population, Generation, Midparent_ID, Parental_Midpoint_TL)

NY_Offspring_TL <- h2_filtered %>%
  filter(Generation == "N3" & Population == "New York") %>%
  select(Population, Generation, Midparent_ID, RelativeTail) %>%
  rename("Offspring_RelTL" = "RelativeTail")

NY_parents_offspring_RelTL <- full_join(NY_Parental_midpoint_TL, NY_Offspring_TL, by = "Midparent_ID")

# ggplot(data = NY_parents_offspring_RelTL, aes(x=Parental_Midpoint_TL, y = Offspring_RelTL)) +
#   geom_point(na.rm = FALSE) +
#   geom_smooth(method = "lm")

# Model
lm.NY_parents_offspring_RelTL <- lm(Offspring_RelTL ~ Parental_Midpoint_TL, data = NY_parents_offspring_RelTL)
#check_model(lm.NY_parents_offspring_RelTL) # looks good!
#car::Anova(lm.NY_parents_offspring_RelTL)

NY_parents_offspring_RelTL_h2 <- signif(as.double(summary(lm.NY_parents_offspring_RelTL)$coef[[2]]), digits = 2) # heritability
NY_parents_offspring_RelTL_se <- signif(as.double(summary(lm.NY_parents_offspring_RelTL)$coef[[4]]), digits = 2) # stderr
NY_parents_offspring_RelTL_pval <- signif(as.double(summary(lm.NY_parents_offspring_RelTL)$coef[[8]]), digits = 2) # p-value


# Brazil
BZ_Parental_midpoint_TL <- midparent_values %>%
  filter(Population == "Brazil") %>%
  select(Population, Generation, Midparent_ID, Parental_Midpoint_TL)

BZ_Offspring_TL <- h2_filtered %>%
  filter(Generation == "N3" & Population == "Brazil") %>%
  select(Population, Generation, Midparent_ID, RelativeTail) %>%
  rename("Offspring_RelTL" = "RelativeTail")

BZ_parents_offspring_RelTL <- inner_join(BZ_Parental_midpoint_TL, BZ_Offspring_TL, by = "Midparent_ID")

# ggplot(data = BZ_parents_offspring_RelTL, aes(x=Parental_Midpoint_TL, y = Offspring_RelTL)) +
#   geom_point(na.rm = FALSE) +
#   geom_smooth(method = "lm")

# Model
lm.BZ_parents_offspring_RelTL <- lm(Offspring_RelTL ~ Parental_Midpoint_TL, data = BZ_parents_offspring_RelTL)
#check_model(lm.BZ_parents_offspring_RelTL) # looks good!
#car::Anova(lm.BZ_parents_offspring_RelTL)

BZ_parents_offspring_RelTL_h2 <- signif(as.double(summary(lm.BZ_parents_offspring_RelTL)$coef[[2]]), digits = 2) # heritability
BZ_parents_offspring_RelTL_se <- signif(as.double(summary(lm.BZ_parents_offspring_RelTL)$coef[[4]]), digits = 2) # stderr
BZ_parents_offspring_RelTL_pval <- signif(as.double(summary(lm.BZ_parents_offspring_RelTL)$coef[[8]]), digits = 2) # p-value


### Ear Length

# New York
NY_Parental_midpoint_EL <- midparent_values %>%
  filter(Population == "New York") %>%
  select(Population, Generation, Midparent_ID, Parental_Midpoint_EL)

NY_Offspring_EL <- h2_filtered %>%
  filter(Generation == "N3" & Population == "New York") %>%
  select(Population, Generation, Midparent_ID, RelativeEar) %>%
  rename("Offspring_RelEL" = "RelativeEar")

NY_parents_offspring_RelEL <- inner_join(NY_Parental_midpoint_EL, NY_Offspring_EL, by = "Midparent_ID")

# ggplot(data = NY_parents_offspring_RelEL, aes(x=Parental_Midpoint_EL, y = Offspring_RelEL)) +
#   geom_point(na.rm = FALSE) +
#   geom_smooth(method = "lm")

# Model
lm.NY_parents_offspring_RelEL <- lm(Offspring_RelEL ~ Parental_Midpoint_EL, data = NY_parents_offspring_RelEL)
#check_model(lm.NY_parents_offspring_RelEL) # looks good
#car::Anova(lm.NY_parents_offspring_RelEL)

NY_parents_offspring_RelEL_h2 <- signif(as.double(summary(lm.NY_parents_offspring_RelEL)$coef[[2]]), digits = 2) # heritability
NY_parents_offspring_RelEL_se <- signif(as.double(summary(lm.NY_parents_offspring_RelEL)$coef[[4]]), digits = 2) # stderr
NY_parents_offspring_RelEL_pval <- signif(as.double(summary(lm.NY_parents_offspring_RelEL)$coef[[8]]), digits = 2) # p-value


# Brazil
BZ_Parental_midpoint_EL <- midparent_values %>%
  filter(Population == "Brazil") %>%
  select(Population, Generation, Midparent_ID, Parental_Midpoint_EL)

BZ_Offspring_EL <- h2_filtered %>%
  filter(Generation == "N3" & Population == "Brazil") %>%
  select(Population, Generation, Midparent_ID, RelativeEar) %>%
  rename("Offspring_RelEL" = "RelativeEar")

BZ_parents_offspring_RelEL <- inner_join(BZ_Parental_midpoint_EL, BZ_Offspring_EL, by = "Midparent_ID")

# ggplot(data = BZ_parents_offspring_RelEL, aes(x=Parental_Midpoint_EL, y = Offspring_RelEL)) +
#   geom_point(na.rm = FALSE) +
#   geom_smooth(method = "lm")

# Model
lm.BZ_parents_offspring_RelEL <- lm(Offspring_RelEL ~ Parental_Midpoint_EL, data = BZ_parents_offspring_RelEL)
#check_model(lm.BZ_parents_offspring_RelEL) # looks good
#car::Anova(lm.BZ_parents_offspring_RelEL)

BZ_parents_offspring_RelEL_h2 <- signif(as.double(summary(lm.BZ_parents_offspring_RelEL)$coef[[2]]), digits = 2) # heritability
BZ_parents_offspring_RelEL_se <- signif(as.double(summary(lm.BZ_parents_offspring_RelEL)$coef[[4]]), digits = 2) # stderr
BZ_parents_offspring_RelEL_pval <- signif(as.double(summary(lm.BZ_parents_offspring_RelEL)$coef[[8]]), digits = 2) # p-value


################################################################################
# Model slopes of parent-offspring regressions, broken down by parental sex --- BW
################################################################################

### Offspring body weight against Brazil moms

BZ_moms_BW <- h2_filtered %>%
  filter(Sex == "Female" & Population == "Brazil" & Generation == "N2") %>%
  select(Population, Generation, Mother_Line, Midparent_ID, Body_Weight_g) %>%
  rename("Parental_Generation" = "Generation",
         "Parental_Population" = "Population",
         "Mother_BW" = "Body_Weight_g")

BZ_offspring_moms_BW <- h2_filtered %>%
  filter(Population == "Brazil" & Generation == "N3") %>%
  select(Population, Sex, Generation, Mother_Line, Midparent_ID, Body_Weight_g) %>%
  rename("Offspring_Generation" = "Generation",
         "Offspring_BW" = "Body_Weight_g",
         "Offspring_Population" = "Population",
         "Offspring_Sex" = "Sex")

BZ_moms_offspring_BW <- inner_join(BZ_moms_BW, BZ_offspring_moms_BW, by = "Midparent_ID")

# ggplot(data = BZ_moms_offspring_BW, aes(x=Mother_BW, y = Offspring_BW)) +
#   geom_point(na.rm = FALSE) +
#   geom_smooth(method = "lm")

# Model
lm.BZ_moms_offspring_BW <- lm(Offspring_BW ~ Mother_BW, data = BZ_moms_offspring_BW)
#check_model(lm.BZ_moms_offspring_BW) # looks good
#car::Anova(lm.BZ_moms_offspring_BW)

BZ_moms_offspring_BW_h2 <- 2*(signif(as.double(summary(lm.BZ_moms_offspring_BW)$coef[[2]]), digits = 2)) # heritability
BZ_moms_offspring_BW_se <- 2*(signif(as.double(summary(lm.BZ_moms_offspring_BW)$coef[[4]]), digits = 2)) # stderr
BZ_moms_offspring_BW_pval <- signif(as.double(summary(lm.BZ_moms_offspring_BW)$coef[[8]]), digits = 2) # p-value


### Offspring body weight against NY moms

NY_moms_BW <- h2_filtered %>%
  filter(Sex == "Female" & Population == "New York" & Generation == "N2") %>%
  select(Population, Generation, Mother_Line, Midparent_ID, Body_Weight_g) %>%
  rename("Parental_Generation" = "Generation",
         "Parental_Population" = "Population",
         "Mother_BW" = "Body_Weight_g")

NY_offspring_moms_BW <- h2_filtered %>%
  filter(Population == "New York" & Generation == "N3") %>%
  select(Population, Sex, Generation, Mother_Line, Midparent_ID, Body_Weight_g) %>%
  rename("Offspring_Generation" = "Generation",
         "Offspring_BW" = "Body_Weight_g",
         "Offspring_Population" = "Population",
         "Offspring_Sex" = "Sex")

NY_moms_offspring_BW <- inner_join(NY_moms_BW, NY_offspring_moms_BW, by = "Midparent_ID")

# ggplot(data = NY_moms_offspring_BW, aes(x=Mother_BW, y = Offspring_BW)) +
#   geom_point(na.rm = FALSE) +
#   geom_smooth(method = "lm")

# Model
lm.NY_moms_offspring_BW <- lm(Offspring_BW ~ Mother_BW, data = NY_moms_offspring_BW)
#check_model(lm.NY_moms_offspring_BW) # looks good
#car::Anova(lm.NY_moms_offspring_BW)

NY_moms_offspring_BW_h2 <- 2*(signif(as.double(summary(lm.NY_moms_offspring_BW)$coef[[2]]), digits = 2)) # heritability
NY_moms_offspring_BW_se <- 2*(signif(as.double(summary(lm.NY_moms_offspring_BW)$coef[[4]]), digits = 2)) # stderr
NY_moms_offspring_BW_pval <- signif(as.double(summary(lm.NY_moms_offspring_BW)$coef[[8]]), digits = 1) # p-value


### Offspring body weight against Brazil dads

BZ_dads_BW <- h2_filtered %>%
  filter(Sex == "Male" & Population == "Brazil" & Generation == "N2") %>%
  select(Population, Generation, Father_Line, Midparent_ID, Body_Weight_g) %>%
  rename("Parental_Generation" = "Generation",
         "Parental_Population" = "Population",
         "Dad_BW" = "Body_Weight_g")

BZ_offspring_dads_BW <- h2_filtered %>%
  filter(Population == "Brazil" & Generation == "N3") %>%
  select(Population, Sex, Generation, Father_Line, Midparent_ID, Body_Weight_g) %>%
  rename("Offspring_Generation" = "Generation",
         "Offspring_BW" = "Body_Weight_g",
         "Offspring_Population" = "Population",
         "Offspring_Sex" = "Sex")

BZ_dads_offspring_BW <- inner_join(BZ_dads_BW, BZ_offspring_dads_BW, by = "Midparent_ID")

# ggplot(data = BZ_dads_offspring_BW, aes(x=Dad_BW, y = Offspring_BW)) +
#   geom_point(na.rm = FALSE) +
#   geom_smooth(method = "lm")

# Model
lm.BZ_dads_offspring_BW <- lm(Offspring_BW ~ Dad_BW, data = BZ_dads_offspring_BW)
#check_model(lm.BZ_dads_offspring_BW) # looks good
#car::Anova(lm.BZ_dads_offspring_BW)

BZ_dads_offspring_BW_h2 <- 2*(signif(as.double(summary(lm.BZ_dads_offspring_BW)$coef[[2]]), digits = 2)) # heritability
BZ_dads_offspring_BW_se <- 2*(signif(as.double(summary(lm.BZ_dads_offspring_BW)$coef[[4]]), digits = 2)) # stderr
BZ_dads_offspring_BW_pval <- signif(as.double(summary(lm.BZ_dads_offspring_BW)$coef[[8]]), digits = 2) # pval


### Offspring body weight against NY dads

NY_dads_BW <- h2_filtered %>%
  filter(Sex == "Male" & Population == "New York" & Generation == "N2") %>%
  select(Population, Generation, Father_Line, Midparent_ID, Body_Weight_g) %>%
  rename("Parental_Generation" = "Generation",
         "Parental_Population" = "Population",
         "Dad_BW" = "Body_Weight_g")

NY_offspring_dads_BW <- h2_filtered %>%
  filter(Population == "New York" & Generation == "N3") %>%
  select(Population, Sex, Generation, Father_Line, Midparent_ID, Body_Weight_g) %>%
  rename("Offspring_Generation" = "Generation",
         "Offspring_BW" = "Body_Weight_g",
         "Offspring_Population" = "Population",
         "Offspring_Sex" = "Sex")

NY_dads_offspring_BW <- inner_join(NY_dads_BW, NY_offspring_dads_BW, by = "Midparent_ID")

# ggplot(data = NY_dads_offspring_BW, aes(x=Dad_BW, y = Offspring_BW)) +
#   geom_point(na.rm = FALSE) +
#   geom_smooth(method = "lm")

# Model
lm.NY_dads_offspring_BW <- lm(Offspring_BW ~ Dad_BW, data = NY_dads_offspring_BW)
#check_model(lm.NY_dads_offspring_BW) # looks fine
#car::Anova(lm.NY_dads_offspring_BW)

NY_dads_offspring_BW_h2 <- 2*(signif(as.double(summary(lm.NY_dads_offspring_BW)$coef[[2]]), digits = 2)) # heritability
NY_dads_offspring_BW_se <- 2*(signif(as.double(summary(lm.NY_dads_offspring_BW)$coef[[4]]), digits = 2)) # stderr
NY_dads_offspring_BW_pval <- signif(as.double(summary(lm.NY_dads_offspring_BW)$coef[[8]]), digits = 2) # p-value


################################################################################
# Model slopes of parent-offspring regressions, broken down by parental sex --- TL
################################################################################

### Offspring relative tail length against Brazil moms

BZ_moms_RelTL <- h2_filtered %>%
  filter(Sex == "Female" & Population == "Brazil" & Generation == "N2") %>%
  select(Population, Generation, Mother_Line, Midparent_ID, RelativeTail) %>%
  rename("Parental_Generation" = "Generation",
         "Parental_Population" = "Population",
         "Mother_RelTL" = "RelativeTail")

BZ_offspring_moms_RelTL <- h2_filtered %>%
  filter(Population == "Brazil" & Generation == "N3") %>%
  select(Population, Sex, Generation, Mother_Line, Midparent_ID, RelativeTail) %>%
  rename("Offspring_Generation" = "Generation",
         "Offspring_RelTL" = "RelativeTail",
         "Offspring_Population" = "Population",
         "Offspring_Sex" = "Sex")

BZ_moms_offspring_RelTL <- inner_join(BZ_moms_RelTL, BZ_offspring_moms_RelTL, by = "Midparent_ID")

# ggplot(data = BZ_moms_offspring_RelTL, aes(x=Mother_RelTL, y = Offspring_RelTL)) +
#   geom_point(na.rm = FALSE) +
#   geom_smooth(method = "lm")

# Model
lm.BZ_moms_offspring_RelTL <- lm(Offspring_RelTL ~ Mother_RelTL, data = BZ_moms_offspring_RelTL)
#check_model(lm.BZ_moms_offspring_RelTL) # looks good
#car::Anova(lm.BZ_moms_offspring_RelTL)

BZ_moms_offspring_RelTL_h2 <- 2*(signif(as.double(summary(lm.BZ_moms_offspring_RelTL)$coef[[2]]), digits = 2)) # heritability
BZ_moms_offspring_RelTL_se <- 2*(signif(as.double(summary(lm.BZ_moms_offspring_RelTL)$coef[[4]]), digits = 2)) # stderr
BZ_moms_offspring_RelTL_pval <- signif(as.double(summary(lm.BZ_moms_offspring_RelTL)$coef[[8]]), digits = 2) # p-value


### Offspring relative tail length against NY moms

NY_moms_RelTL <- h2_filtered %>%
  filter(Sex == "Female" & Population == "New York" & Generation == "N2") %>%
  select(Population, Generation, Mother_Line, Midparent_ID, RelativeTail) %>%
  rename("Parental_Generation" = "Generation",
         "Parental_Population" = "Population",
         "Mother_RelTL" = "RelativeTail")

NY_offspring_moms_RelTL <- h2_filtered %>%
  filter(Population == "New York" & Generation == "N3") %>%
  select(Population, Sex, Generation, Mother_Line, Midparent_ID, RelativeTail) %>%
  rename("Offspring_Generation" = "Generation",
         "Offspring_RelTL" = "RelativeTail",
         "Offspring_Population" = "Population",
         "Offspring_Sex" = "Sex")

NY_moms_offspring_RelTL <- inner_join(NY_moms_RelTL, NY_offspring_moms_RelTL, by = "Midparent_ID")

# ggplot(data = NY_moms_offspring_RelTL, aes(x=Mother_RelTL, y = Offspring_RelTL)) +
#   geom_point(na.rm = FALSE) +
#   geom_smooth(method = "lm")

# Model
lm.NY_moms_offspring_RelTL <- lm(Offspring_RelTL ~ Mother_RelTL, data = NY_moms_offspring_RelTL)
#check_model(lm.NY_moms_offspring_RelTL) # looks good
#car::Anova(lm.NY_moms_offspring_RelTL)

NY_moms_offspring_RelTL_h2 <- 2*(signif(as.double(summary(lm.NY_moms_offspring_RelTL)$coef[[2]]), digits = 2)) # heritability
NY_moms_offspring_RelTL_se <- 2*(signif(as.double(summary(lm.NY_moms_offspring_RelTL)$coef[[4]]), digits = 2)) # stderr
NY_moms_offspring_RelTL_pval <- signif(as.double(summary(lm.NY_moms_offspring_RelTL)$coef[[8]]), digits = 1) # p-value


### Offspring relative tail length against Brazil dads

BZ_dads_RelTL <- h2_filtered %>%
  filter(Sex == "Male" & Population == "Brazil" & Generation == "N2") %>%
  select(Population, Generation, Father_Line, Midparent_ID, RelativeTail) %>%
  rename("Parental_Generation" = "Generation",
         "Parental_Population" = "Population",
         "Father_RelTL" = "RelativeTail")

BZ_offspring_dads_RelTL <- h2_filtered %>%
  filter(Population == "Brazil" & Generation == "N3") %>%
  select(Population, Sex, Generation, Father_Line, Midparent_ID, RelativeTail) %>%
  rename("Offspring_Generation" = "Generation",
         "Offspring_RelTL" = "RelativeTail",
         "Offspring_Population" = "Population",
         "Offspring_Sex" = "Sex")

BZ_dads_offspring_RelTL <- inner_join(BZ_dads_RelTL, BZ_offspring_dads_RelTL, by = "Midparent_ID")

# ggplot(data = BZ_dads_offspring_RelTL, aes(x=Father_RelTL, y = Offspring_RelTL)) +
#   geom_point(na.rm = FALSE) +
#   geom_smooth(method = "lm")

# Model
lm.BZ_dads_offspring_RelTL <- lm(Offspring_RelTL ~ Father_RelTL, data = BZ_dads_offspring_RelTL)
#check_model(lm.BZ_dads_offspring_RelTL) # looks good
#car::Anova(lm.BZ_dads_offspring_RelTL)

BZ_dads_offspring_RelTL_h2 <- 2*(signif(as.double(summary(lm.BZ_dads_offspring_RelTL)$coef[[2]]), digits = 2)) # heritability
BZ_dads_offspring_RelTL_se <- 2*(signif(as.double(summary(lm.BZ_dads_offspring_RelTL)$coef[[4]]), digits = 2)) # stderr
BZ_dads_offspring_RelTL_pval <- signif(as.double(summary(lm.BZ_dads_offspring_RelTL)$coef[[8]]), digits = 2) # p-value


### Offspring relative tail length against NY dads

NY_dads_RelTL <- h2_filtered %>%
  filter(Sex == "Male" & Population == "New York" & Generation == "N2") %>%
  select(Population, Generation, Father_Line, Midparent_ID, RelativeTail) %>%
  rename("Parental_Generation" = "Generation",
         "Parental_Population" = "Population",
         "Father_RelTL" = "RelativeTail")

NY_offspring_dads_RelTL <- h2_filtered %>%
  filter(Population == "New York" & Generation == "N3") %>%
  select(Population, Sex, Generation, Father_Line, Midparent_ID, RelativeTail) %>%
  rename("Offspring_Generation" = "Generation",
         "Offspring_RelTL" = "RelativeTail",
         "Offspring_Population" = "Population",
         "Offspring_Sex" = "Sex")

NY_dads_offspring_RelTL <- inner_join(NY_dads_RelTL, NY_offspring_dads_RelTL, by = "Midparent_ID")

# ggplot(data = NY_dads_offspring_RelTL, aes(x=Father_RelTL, y = Offspring_RelTL)) +
#   geom_point(na.rm = FALSE) +
#   geom_smooth(method = "lm")

# Model
lm.NY_dads_offspring_RelTL <- lm(Offspring_RelTL ~ Father_RelTL, data = NY_dads_offspring_RelTL)
#check_model(lm.NY_dads_offspring_RelTL) # looks good
#car::Anova(lm.NY_dads_offspring_RelTL)

NY_dads_offspring_RelTL_h2 <- 2*(signif(as.double(summary(lm.NY_dads_offspring_RelTL)$coef[[2]]), digits = 2)) # heritability
NY_dads_offspring_RelTL_se <- 2*(signif(as.double(summary(lm.NY_dads_offspring_RelTL)$coef[[4]]), digits = 2)) # stderr
NY_dads_offspring_RelTL_pval <- signif(as.double(summary(lm.NY_dads_offspring_RelTL)$coef[[8]]), digits = 2) # p-value


################################################################################
# Model slopes of parent-offspring regressions, broken down by parental sex --- EL
################################################################################

### Offspring relative ear length against Brazil moms

BZ_moms_RelEL <- h2_filtered %>%
  filter(Sex == "Female" & Population == "Brazil" & Generation == "N2") %>%
  select(Population, Generation, Mother_Line, Midparent_ID, RelativeEar) %>%
  rename("Parental_Generation" = "Generation",
         "Parental_Population" = "Population",
         "Mother_RelEL" = "RelativeEar")

BZ_offspring_moms_RelEL <- h2_filtered %>%
  filter(Population == "Brazil" & Generation == "N3") %>%
  select(Population, Sex, Generation, Mother_Line, Midparent_ID, RelativeEar) %>%
  rename("Offspring_Generation" = "Generation",
         "Offspring_RelEL" = "RelativeEar",
         "Offspring_Population" = "Population",
         "Offspring_Sex" = "Sex")

BZ_moms_offspring_RelEL <- inner_join(BZ_moms_RelEL, BZ_offspring_moms_RelEL, by = "Midparent_ID")

# ggplot(data = BZ_moms_offspring_RelEL, aes(x=Mother_RelEL, y = Offspring_RelEL)) +
#   geom_point(na.rm = FALSE) +
#   geom_smooth(method = "lm")

# Model
lm.BZ_moms_offspring_RelEL <- lm(Offspring_RelEL ~ Mother_RelEL, data = BZ_moms_offspring_RelEL)
#check_model(lm.BZ_moms_offspring_RelEL) # looks good
#car::Anova(lm.BZ_moms_offspring_RelEL)

BZ_moms_offspring_RelEL_h2 <- 2*(signif(as.double(summary(lm.BZ_moms_offspring_RelEL)$coef[[2]]), digits = 2)) # heritability
BZ_moms_offspring_RelEL_se <- 2*(signif(as.double(summary(lm.BZ_moms_offspring_RelEL)$coef[[4]]), digits = 2)) # stderr
BZ_moms_offspring_RelEL_pval <- signif(as.double(summary(lm.BZ_moms_offspring_RelEL)$coef[[8]]), digits = 2) # p-value


### Offspring relative ear length against NY moms

NY_moms_RelEL <- h2_filtered %>%
  filter(Sex == "Female" & Population == "New York" & Generation == "N2") %>%
  select(Population, Generation, Mother_Line, Midparent_ID, RelativeEar) %>%
  rename("Parental_Generation" = "Generation",
         "Parental_Population" = "Population",
         "Mother_RelEL" = "RelativeEar")

NY_offspring_moms_RelEL <- h2_filtered %>%
  filter(Population == "New York" & Generation == "N3") %>%
  select(Population, Sex, Generation, Mother_Line, Midparent_ID, RelativeEar) %>%
  rename("Offspring_Generation" = "Generation",
         "Offspring_RelEL" = "RelativeEar",
         "Offspring_Population" = "Population",
         "Offspring_Sex" = "Sex")

NY_moms_offspring_RelEL <- inner_join(NY_moms_RelEL, NY_offspring_moms_RelEL, by = "Midparent_ID")

# ggplot(data = NY_moms_offspring_RelEL, aes(x=Mother_RelEL, y = Offspring_RelEL)) +
#   geom_point(na.rm = FALSE) +
#   geom_smooth(method = "lm")

# Model
lm.NY_moms_offspring_RelEL <- lm(Offspring_RelEL ~ Mother_RelEL, data = NY_moms_offspring_RelEL)
#check_model(lm.NY_moms_offspring_RelEL) # looks good
#car::Anova(lm.NY_moms_offspring_RelEL)

NY_moms_offspring_RelEL_h2 <- 2*(signif(as.double(summary(lm.NY_moms_offspring_RelEL)$coef[[2]]), digits = 2)) # heritability
NY_moms_offspring_RelEL_se <- 2*(signif(as.double(summary(lm.NY_moms_offspring_RelEL)$coef[[4]]), digits = 2)) # stderr
NY_moms_offspring_RelEL_pval <- signif(as.double(summary(lm.NY_moms_offspring_RelEL)$coef[[8]]), digits = 1) # p-value


### Offspring relative ear length against Brazil dads

BZ_dads_RelEL <- h2_filtered %>%
  filter(Sex == "Male" & Population == "Brazil" & Generation == "N2") %>%
  select(Population, Generation, Father_Line, Midparent_ID, RelativeEar) %>%
  rename("Parental_Generation" = "Generation",
         "Parental_Population" = "Population",
         "Father_RelEL" = "RelativeEar")

BZ_offspring_dads_RelEL <- h2_filtered %>%
  filter(Population == "Brazil" & Generation == "N3") %>%
  select(Population, Sex, Generation, Father_Line, Midparent_ID, RelativeEar) %>%
  rename("Offspring_Generation" = "Generation",
         "Offspring_RelEL" = "RelativeEar",
         "Offspring_Population" = "Population",
         "Offspring_Sex" = "Sex")

BZ_dads_offspring_RelEL <- inner_join(BZ_dads_RelEL, BZ_offspring_dads_RelEL, by = "Midparent_ID")

# ggplot(data = BZ_dads_offspring_RelEL, aes(x=Father_RelEL, y = Offspring_RelEL)) +
#   geom_point(na.rm = FALSE) +
#   geom_smooth(method = "lm")

# Model
lm.BZ_dads_offspring_RelEL <- lm(Offspring_RelEL ~ Father_RelEL, data = BZ_dads_offspring_RelEL)
#check_model(lm.BZ_dads_offspring_RelEL) # looks good
#car::Anova(lm.BZ_dads_offspring_RelEL)

BZ_dads_offspring_RelEL_h2 <- 2*(signif(as.double(summary(lm.BZ_dads_offspring_RelEL)$coef[[2]]), digits = 2)) # heritability
BZ_dads_offspring_RelEL_se <- 2*(signif(as.double(summary(lm.BZ_dads_offspring_RelEL)$coef[[4]]), digits = 2)) # stderr
BZ_dads_offspring_RelEL_pval <- signif(as.double(summary(lm.BZ_dads_offspring_RelEL)$coef[[8]]), digits = 2) # p-value


### Offspring relative ear length against NY dads

NY_dads_RelEL <- h2_filtered %>%
  filter(Sex == "Male" & Population == "New York" & Generation == "N2") %>%
  select(Population, Generation, Father_Line, Midparent_ID, RelativeEar) %>%
  rename("Parental_Generation" = "Generation",
         "Parental_Population" = "Population",
         "Father_RelEL" = "RelativeEar")

NY_offspring_dads_RelEL <- h2_filtered %>%
  filter(Population == "New York" & Generation == "N3") %>%
  select(Population, Sex, Generation, Father_Line, Midparent_ID, RelativeEar) %>%
  rename("Offspring_Generation" = "Generation",
         "Offspring_RelEL" = "RelativeEar",
         "Offspring_Population" = "Population",
         "Offspring_Sex" = "Sex")

NY_dads_offspring_RelEL <- inner_join(NY_dads_RelEL, NY_offspring_dads_RelEL, by = "Midparent_ID")

# ggplot(data = NY_dads_offspring_RelEL, aes(x=Father_RelEL, y = Offspring_RelEL)) +
#   geom_point(na.rm = FALSE) +
#   geom_smooth(method = "lm")

# Model
lm.NY_dads_offspring_RelEL <- lm(Offspring_RelEL ~ Father_RelEL, data = NY_dads_offspring_RelEL)
#check_model(lm.NY_dads_offspring_RelEL) # looks good
#car::Anova(lm.NY_dads_offspring_RelEL)

NY_dads_offspring_RelEL_h2 <- 2*(signif(as.double(summary(lm.NY_dads_offspring_RelEL)$coef[[2]]), digits = 2)) # heritability
NY_dads_offspring_RelEL_se <- 2*(signif(as.double(summary(lm.NY_dads_offspring_RelEL)$coef[[4]]), digits = 2)) # stderr
NY_dads_offspring_RelEL_pval <- signif(as.double(summary(lm.NY_dads_offspring_RelEL)$coef[[8]]), digits = 2) # p-value

