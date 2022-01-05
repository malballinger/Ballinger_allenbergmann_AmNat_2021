#!/usr/bin/env Rscript --vanilla

##############################################################

# Author: Mallory A. Ballinger

# This script plots body mass and relative extremity length across
# mean temperature from wild-caught house mice collected across North and
# South America. Data are from VertNet.org and were cleaned using
# the script ./clean_VertNetMetadata.R. Mean temperature data were
# downloaded from WorldClim.org and rasters were extracted in ./basic_raster_extraction.R.
# This script generates Supplemental Figure 1 in Ballinger_AmNat_2021.


##############################################################
# Required packages
##############################################################

rm(list = ls()) # clear R's environment
library(tidyverse)
library(here)
library(cowplot)
library(ggtext)
library(glue)
library(scales)
library(report)
library(patchwork)

set.seed(19910118) # so that jitter plots stay in same jittered positions


##############################################################
# Import data
##############################################################

VertNetMetadata <- read_csv(here("data/processed/VertNetMetadata_Mus_2021-03-18_mean_temp.csv")) %>%
  select(-1, -2) %>%
  select (-mean_temp1, -mean_temp2, -mean_temp3, -mean_temp4, -mean_temp5, -mean_temp6, -mean_temp7,
          -mean_temp8, -mean_temp9, -mean_temp10, -mean_temp11, -mean_temp12) %>%
  mutate(Sex = fct_recode(Sex, "Male" = "male", "Female" = "female")) %>%
  mutate(Sex = fct_relevel(Sex, "Male", "Female")) # puts males before females


##############################################################
# How correlated is mean temp and latitude with the house mouse dataset?
##############################################################

#check_model(lm(mean_temp ~ Absolute_Latitude, data = VertNetMetadata))

TempVsLat <-
ggplot(data=VertNetMetadata, aes(x=Absolute_Latitude, y=mean_temp)) +
  geom_point() + geom_smooth(method = "lm")

TempVsLat_corr <-
cor.test(x = VertNetMetadata$Absolute_Latitude,
         y = VertNetMetadata$mean_temp,
         method = 'spearman', exact = FALSE,
         adjust="fdr", alpha=0.5)


##############################################################
# Apply filtering and calculate residuals
##############################################################

# Based on outlier tests (see ./model_VertNetMetadata_relative.R), any tail length
# less than 20 and greater than 120 are extreme outliers
VertNet_filtered <- VertNetMetadata %>%
  mutate(Tail_Length_mm = ifelse(Tail_Length_mm < 20, NA, Tail_Length_mm),
         Tail_Length_mm = ifelse(Tail_Length_mm > 120, NA, Tail_Length_mm)) %>%
  mutate(RelativeTail = (Tail_Length_mm) / (Body_Weight_g))


# Based on outlier tests (see ./model_VertNetMetadata_relative.R), any ear length
# greater than 30 is extreme outliers
VertNet_filtered <- VertNet_filtered %>%
  mutate(Ear_Length_mm = ifelse(Ear_Length_mm > 30, NA, Ear_Length_mm)) %>%
  mutate(RelativeEar = (Ear_Length_mm) / (Body_Weight_g))


##############################################################
# Get values from correlation analyses
##############################################################

## Bergmann's rule

Male_Bergmann_VertNet_temp <- VertNetMetadata %>%
  filter(Sex == "Male")
#check_model(lm(Body_Weight_g ~ mean_temp, data = Male_Bergmann_VertNet_temp)) # looks good

Male_Bergmann_VertNet_temp_N <- comma(length(which(!is.na(Male_Bergmann_VertNet_temp$Body_Weight_g))))

cor.Berg.Male.VertNet_temp <- cor.test(x = Male_Bergmann_VertNet_temp$mean_temp,
                                  y = Male_Bergmann_VertNet_temp$Body_Weight_g,
                                  method = 'spearman', exact = FALSE,
                                  adjust="fdr", alpha=0.5)

cor.Berg.Male.VertNet_temp_corr <- signif(as.double(cor.Berg.Male.VertNet_temp$estimate), digits = 2)
cor.Berg.Male.VertNet_temp_pval <- signif(as.double(cor.Berg.Male.VertNet_temp$p.value), digits = 2)


Female_Bergmann_VertNet_temp <- VertNetMetadata %>%
  filter(Sex == "Female")
#check_model(lm(Body_Weight_g ~ mean_temp, data = Female_Bergmann_VertNet_temp)) # looks good

Female_Bergmann_VertNet_temp_N <- comma(length(which(!is.na(Female_Bergmann_VertNet_temp$Body_Weight_g))))

cor.Berg.Female.VertNet_temp <- cor.test(x = Female_Bergmann_VertNet_temp$mean_temp,
                                       y = Female_Bergmann_VertNet_temp$Body_Weight_g,
                                       method = 'spearman', exact = FALSE,
                                       adjust="fdr", alpha=0.5)

cor.Berg.Female.VertNet_temp_corr <- signif(as.double(cor.Berg.Female.VertNet_temp$estimate), digits = 2)
cor.Berg.Female.VertNet_temp_pval <- signif(as.double(cor.Berg.Female.VertNet_temp$p.value), digits = 2)


Male_Bergmann_Adult_VertNet_temp <- Male_Bergmann_VertNet_temp %>%
  filter(Lifestage == "adult")
#check_model(lm(Body_Weight_g ~ mean_temp, data = Male_Bergmann_Adult_VertNet_temp)) # looks good

Male_Bergmann_Adult_VertNet_temp_N <- comma(length(which(!is.na(Male_Bergmann_Adult_VertNet_temp$Body_Weight_g))))

cor.Berg.Male.Adult.VertNet_temp <- cor.test(x = Male_Bergmann_Adult_VertNet_temp$mean_temp,
                                       y = Male_Bergmann_Adult_VertNet_temp$Body_Weight_g,
                                       method = 'spearman', exact = FALSE,
                                       adjust="fdr", alpha=0.5)

cor.Berg.Male.Adult.VertNet_temp_corr <- signif(as.double(cor.Berg.Male.Adult.VertNet_temp$estimate), digits = 2)
cor.Berg.Male.Adult.VertNet_temp_pval <- signif(as.double(cor.Berg.Male.Adult.VertNet_temp$p.value), digits = 2)



## Allen's rule - TAIL

Male_Tail_VertNet_temp <- VertNet_filtered %>%
  filter(Sex == "Male")
#check_model(lm(RelativeTail ~ mean_temp, data = Male_Tail_VertNet_temp)) # looks good

Male_Tail_VertNet_temp_N <- comma(length(which(!is.na(Male_Tail_VertNet_temp$RelativeTail))))

cor.Tail.Male.VertNet_temp <- cor.test(x = Male_Tail_VertNet_temp$mean_temp,
                                       y = Male_Tail_VertNet_temp$RelativeTail,
                                       method = 'spearman', exact = FALSE,
                                       adjust="fdr", alpha=0.5)

cor.Tail.Male.VertNet_temp_corr <- signif(as.double(cor.Tail.Male.VertNet_temp$estimate), digits = 2)
cor.Tail.Male.VertNet_temp_pval <- signif(as.double(cor.Tail.Male.VertNet_temp$p.value), digits = 2)


Female_Tail_VertNet_temp <- VertNet_filtered %>%
   filter(Sex == "Female")
#check_model(lm(RelativeTail ~ mean_temp, data = Female_Tail_VertNet_temp)) # looks good

Female_Tail_VertNet_temp_N <- comma(length(which(!is.na(Female_Tail_VertNet_temp$RelativeTail))))

cor.Tail.Female.VertNet_temp <- cor.test(x = Female_Tail_VertNet_temp$mean_temp,
                                         y = Female_Tail_VertNet_temp$RelativeTail,
                                         method = 'spearman', exact = FALSE,
                                         adjust="fdr", alpha=0.5)

cor.Tail.Female.VertNet_temp_corr <- signif(as.double(cor.Tail.Female.VertNet_temp$estimate), digits = 2)
cor.Tail.Female.VertNet_temp_pval <- signif(as.double(cor.Tail.Female.VertNet_temp$p.value), digits = 1)


Male_Tail_Adult_VertNet_temp <- Male_Tail_VertNet_temp %>%
  filter(Lifestage == "adult")
#check_model(lm(RelativeTail ~ mean_temp, data = Male_Tail_Adult_VertNet_temp)) # looks good

Male_Tail_Adult_VertNet_temp_N <- comma(length(which(!is.na(Male_Tail_Adult_VertNet_temp$RelativeTail))))

cor.Tail.Male.Adult.VertNet_temp <- cor.test(x = Male_Tail_Adult_VertNet_temp$mean_temp,
                                       y = Male_Tail_Adult_VertNet_temp$RelativeTail,
                                       method = 'spearman', exact = FALSE,
                                       adjust="fdr", alpha=0.5)

cor.Tail.Male.Adult.VertNet_temp_corr <- signif(as.double(cor.Tail.Male.Adult.VertNet_temp$estimate), digits = 2)
cor.Tail.Male.Adult.VertNet_temp_pval <- signif(as.double(cor.Tail.Male.Adult.VertNet_temp$p.value), digits = 2)



## Allen's rule - EAR

Male_Ear_VertNet_temp <- VertNet_filtered %>%
  filter(Sex == "Male")
#check_model(lm(RelativeEar ~ mean_temp, data = Male_Ear_VertNet_temp)) # looks good

Male_Ear_VertNet_temp_N <- comma(length(which(!is.na(Male_Ear_VertNet_temp$RelativeEar))))

cor.Ear.Male.VertNet_temp <- cor.test(x = Male_Ear_VertNet_temp$mean_temp,
                                       y = Male_Ear_VertNet_temp$RelativeEar,
                                       method = 'spearman', exact = FALSE,
                                       adjust="fdr", alpha=0.5)

cor.Ear.Male.VertNet_temp_corr <- signif(as.double(cor.Ear.Male.VertNet_temp$estimate), digits = 2)
cor.Ear.Male.VertNet_temp_pval <- signif(as.double(cor.Ear.Male.VertNet_temp$p.value), digits = 1)


Female_Ear_VertNet_temp <- VertNet_filtered %>%
  filter(Sex == "Female")
#check_model(lm(RelativeEar ~ mean_temp, data = Female_Ear_VertNet_temp)) # looks good

Female_Ear_VertNet_temp_N <- comma(length(which(!is.na(Female_Ear_VertNet_temp$RelativeEar))))

cor.Ear.Female.VertNet_temp <- cor.test(x = Female_Ear_VertNet_temp$mean_temp,
                                         y = Female_Ear_VertNet_temp$RelativeEar,
                                         method = 'spearman', exact = FALSE,
                                         adjust="fdr", alpha=0.5)

cor.Ear.Female.VertNet_temp_corr <- signif(as.double(cor.Ear.Female.VertNet_temp$estimate), digits = 2)
cor.Ear.Female.VertNet_temp_pval <- signif(as.double(cor.Ear.Female.VertNet_temp$p.value), digits = 1)


Male_Ear_Adult_VertNet_temp <- Male_Ear_VertNet_temp %>%
  filter(Lifestage == "adult")
#check_model(lm(RelativeEar ~ mean_temp, data = Male_Ear_Adult_VertNet_temp)) # looks good

Male_Ear_Adult_VertNet_temp_N <- comma(length(which(!is.na(Male_Ear_Adult_VertNet_temp$RelativeEar))))

cor.Ear.Male.Adult.VertNet_temp <- cor.test(x = Male_Ear_Adult_VertNet_temp$mean_temp,
                                      y = Male_Ear_Adult_VertNet_temp$RelativeEar,
                                      method = 'spearman', exact = FALSE,
                                      adjust="fdr", alpha=0.5)

cor.Ear.Male.Adult.VertNet_temp_corr <- signif(as.double(cor.Ear.Male.Adult.VertNet_temp$estimate), digits = 2)
cor.Ear.Male.Adult.VertNet_temp_pval <- signif(as.double(cor.Ear.Male.Adult.VertNet_temp$p.value), digits = 1)


##############################################################
# Save results for each sex
##############################################################

Berg_VertNet_M_temp <- glue("males (rho = {cor.Berg.Male.VertNet_temp_corr},  \\
                      *P* = {cor.Berg.Male.VertNet_temp_pval}, \\
                       *n* = {Male_Bergmann_VertNet_temp_N})")
Berg_VertNet_F_temp <- glue("females (rho = {cor.Berg.Female.VertNet_temp_corr}, \\
                       *P* = {cor.Berg.Female.VertNet_temp_pval}, \\
                       *n* = {Female_Bergmann_VertNet_temp_N})")
Berg_VertNet_Adult_M_temp <- glue("rho = {cor.Berg.Male.Adult.VertNet_temp_corr}, \\
                             *P* < 0.0001, \\
                             *n* = {Male_Bergmann_Adult_VertNet_temp_N}")

Tail_VertNet_M_temp <- glue ("males (rho = {cor.Tail.Male.VertNet_temp_corr}, \\
                       *P* < 0.0001, \\
                       *n* = {Male_Tail_VertNet_temp_N})")
Tail_VertNet_F_temp <- glue("females (rho = {cor.Tail.Female.VertNet_temp_corr}, \\
                       *P* = {cor.Tail.Female.VertNet_temp_pval}, \\
                       *n* = {Female_Tail_VertNet_temp_N})")
Tail_VertNet_Adult_M_temp <- glue("rho = {cor.Tail.Male.Adult.VertNet_temp_corr}, \\
                             *P* < 0.0001, \\
                             *n* = {Male_Tail_Adult_VertNet_temp_N}")

Ear_VertNet_M_temp <- glue("males (rho = {cor.Ear.Male.VertNet_temp_corr}, \\
                      *P* < 0.001, \\
                      *n* = {Male_Ear_VertNet_temp_N})")
Ear_VertNet_F_temp <- glue("females (rho = {cor.Ear.Female.VertNet_temp_corr}, \\
                      *P* = {cor.Ear.Female.VertNet_temp_pval}, \\
                      *n* = {Female_Ear_VertNet_temp_N})")
Ear_VertNet_Adult_M_temp <- glue("rho = {cor.Ear.Male.Adult.VertNet_temp_corr}, \\
                            *P* < 0.0001, \\
                            *n* = {Male_Ear_Adult_VertNet_temp_N}")


##############################################################
# Combine all 6 plots for manuscript
##############################################################

Berg_VertNet_temp <-
  ggplot(data = VertNetMetadata, aes(x = mean_temp, y = Body_Weight_g)) +
  geom_jitter(aes(fill = Sex), size = 1.85, stroke = 0.25, height = 0, width = 0.2, shape = 21, alpha = 0.9, color = "black") +
  geom_smooth(aes(linetype = Sex, color = Sex), method = "lm", se = FALSE, size = 0.5) +
  scale_color_manual(labels=c(Berg_VertNet_M_temp, Berg_VertNet_F_temp),
                     values=c("black", "darkgray")) +
  scale_fill_manual(labels=c(Berg_VertNet_M_temp, Berg_VertNet_F_temp),
                    values=c("black", "white")) +
  scale_linetype_manual(labels=c(Berg_VertNet_M_temp, Berg_VertNet_F_temp),
                        values = c("solid", "solid")) +
  scale_x_continuous(breaks = seq(from=0, to=30, by=5),
                     labels = seq(from=0, to=30, by=5),
                     limits = c(0,30)) +
  scale_y_continuous(breaks = seq(from=0, to=40, by=10),
                     labels = seq(from=0, to=40, by=10),
                     limits = c(0,46)) +
  theme_bw() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 0), size = 10, family = "Palatino"),
        axis.title.y = element_text(margin = margin(r = 3), size = 10, family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.title = element_blank(),
        legend.position = c(0.435, 0.95),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key.size = unit(0.25, "lines"),
        legend.text = element_markdown(size=6.5, family = "Palatino"),
        legend.key = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.25, 0.5, 0, 0.5), "cm"), #top, right, bottom, left
        plot.title = element_text(size = 11, face = "bold.italic", hjust = 0.5, vjust = 0.1, family = "Palatino")) +
  labs(x = NULL,
       y = "Body Mass (g)",
       title = "VertNet Metadata")


Tail_VertNet_temp <-
  ggplot(data = VertNet_filtered, aes(x = mean_temp, y = RelativeTail)) +
  geom_jitter(aes(fill = Sex), size = 1.85, stroke = 0.25, height = 0, width = 0.2, shape = 21, alpha = 0.9, color = "black") +
  geom_smooth(aes(linetype = Sex, color = Sex), method = "lm", se = FALSE, size = 0.5) +
  scale_color_manual(labels=c(Tail_VertNet_M_temp, Tail_VertNet_F_temp),
                     values=c("black", "darkgray")) +
  scale_fill_manual(labels=c(Tail_VertNet_M_temp, Tail_VertNet_F_temp),
                    values=c("black", "white")) +
  scale_linetype_manual(labels=c(Tail_VertNet_M_temp, Tail_VertNet_F_temp),
                        values = c("solid", "solid")) +
  scale_x_continuous(breaks = seq(from=0, to=30, by=5),
                     labels = seq(from=0, to=30, by=5),
                     limits = c(0,30)) +
  scale_y_continuous(breaks = seq(from=0, to=18, by=5),
                     labels = seq(from=0, to=18, by=5),
                     limits = c(NA,18)) +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 0), size = 10, family = "Palatino"),
        axis.title.y = element_text(margin = margin(r = 3), size = 10, family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.title = element_blank(),
        legend.position = c(0.41, 0.95),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key.size = unit(0.25, "lines"),
        legend.text = element_markdown(size=6.5, family = "Palatino"),
        legend.key = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.25, 0.5, 0.25, 0.5), "cm")) +
  labs(x = NULL,
       y = "Relative Tail Length")


Ear_VertNet_temp <-
  ggplot(data = VertNet_filtered, aes(x = mean_temp, y = RelativeEar)) +
  geom_jitter(aes(fill = Sex), size = 1.85, stroke = 0.25, height = 0, width = 0.2, shape = 21, alpha = 0.9, color = "black") +
  geom_smooth(aes(linetype = Sex, color = Sex), method = "lm", se = FALSE, size = 0.5) +
  scale_color_manual(labels=c(Ear_VertNet_M_temp, Ear_VertNet_F_temp),
                     values=c("black", "darkgray")) +
  scale_fill_manual(labels=c(Ear_VertNet_M_temp, Ear_VertNet_F_temp),
                    values=c("black", "white")) +
  scale_linetype_manual(labels=c(Ear_VertNet_M_temp, Ear_VertNet_F_temp),
                        values = c("solid", "solid")) +
  scale_x_continuous(breaks = seq(from=0, to=30, by=5),
                     labels = seq(from=0, to=30, by=5),
                     limits = c(0,30)) +
  scale_y_continuous(breaks = seq(from=0, to=3.5, by=1),
                     labels = seq(from=0, to=3.5, by=1),
                     limits = c(NA,3.2)) +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_markdown(margin = margin(t = 10), size = 10, family = "Palatino"),
        axis.title.y = element_text(margin = margin(r = 0), size = 10, family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.title = element_blank(),
        legend.position = c(0.395, 0.95),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key.size = unit(0.25, "lines"),
        legend.text = element_markdown(size=6.5, family = "Palatino"),
        legend.key = element_rect(fill = "transparent"),
        plot.margin = unit(c(0, 0.5, 0.25, 0.5), "cm")) +
  labs(x = "Mean Temperature (<sup>o</sup>C)",
       y = "Relative Ear Length")


##############################################################
# Adult males only from VertNet
##############################################################

Berg_Adult_Male_VertNet_temp <-
  ggplot(data = Male_Bergmann_Adult_VertNet_temp, aes(x = mean_temp, y = Body_Weight_g, color = "black", fill = "black")) +
  geom_jitter(size = 2.3, stroke = 0.25, height = 0, width = 0.2, shape = 21, alpha = 0.75, show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE, size = 0.5, show.legend = FALSE) +
  scale_color_manual(labels=c(Tail_VertNet_Adult_M_temp),
                     values=c("white")) +
  scale_fill_manual(labels=c(Tail_VertNet_Adult_M_temp),
                    values=c("black")) +
  scale_x_continuous(breaks = seq(from=0, to=30, by=5),
                     labels = seq(from=0, to=30, by=5),
                     limits = c(0,30)) +
  scale_y_continuous(breaks = seq(from=5, to=35, by=10),
                     labels = seq(from=5, to=35, by=10),
                     limits = c(5,35)) +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10), size = 10, family = "Palatino"),
        axis.title.y = element_text(margin = margin(r = 0), size = 10, family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.position = 'none',
        plot.tag = element_markdown(family = "Palatino", size = 7),
        plot.tag.position = c(0.435,0.855),
        plot.margin = unit(c(0.25, 0.5, 0, 0.5), "cm"),
        plot.title = element_text(size = 11, face = "bold.italic", hjust = 0.5, vjust = 0.1, family = "Palatino")) +
  labs(x = NULL,
       y = "Body Mass (g)",
       tag = Berg_VertNet_Adult_M_temp,
       title = "VertNet Adult Males")


Tail_Adult_Male_VertNet_temp <-
  ggplot(data = Male_Tail_Adult_VertNet_temp, aes(x = mean_temp, y = RelativeTail, color = "black", fill = "black")) +
  geom_jitter(size = 2.3, stroke = 0.25, height = 0, width = 0.2, shape = 21, alpha = 0.75, show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE, size = 0.5, show.legend = FALSE) +
  scale_color_manual(labels=c(Tail_VertNet_Adult_M_temp),
                     values=c("white")) +
  scale_fill_manual(labels=c(Tail_VertNet_Adult_M_temp),
                    values=c("black")) +
  scale_x_continuous(breaks = seq(from=0, to=30, by=5),
                     labels = seq(from=0, to=30, by=5),
                     limits = c(0,30)) +
  scale_y_continuous(breaks = seq(from=1.5, to=11, by=4),
                     labels = seq(from=1.5, to=11, by=4),
                     limits = c(1.5,10.5)) +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10), size = 10, family = "Palatino"),
        axis.title.y = element_text(margin = margin(r = 5), size = 10, family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.position = 'none',
        plot.tag = element_markdown(family = "Palatino", size = 7),
        plot.tag.position = c(0.41,0.95),
        plot.margin = unit(c(0.25, 0.5, 0.25, 0.5), "cm")) +
  labs(x = NULL,
       y = "Relative Tail Length",
       tag = Tail_VertNet_Adult_M_temp)


Ear_Adult_Male_VertNet_temp <-
  ggplot(data = Male_Ear_Adult_VertNet_temp, aes(x = mean_temp, y = RelativeEar, color = "black", fill = "black")) +
  geom_jitter(size = 2.3, stroke = 0.25, height = 0, width = 0.2, shape = 21, alpha = 0.75, show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE, size = 0.5, show.legend = FALSE) +
  scale_color_manual(labels=c(Ear_VertNet_Adult_M_temp),
                     values=c("white")) +
  scale_fill_manual(labels=c(Ear_VertNet_Adult_M_temp),
                    values=c("black")) +
  scale_x_continuous(breaks = seq(from=0, to=30, by=5),
                     labels = seq(from=0, to=30, by=5),
                     limits = c(0,30)) +
  scale_y_continuous(breaks = seq(from=0.4, to=1.7, by=0.5),
                     labels = seq(from=0.4, to=1.7, by=0.5),
                     limits = c(0.4,1.7)) +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_markdown(margin = margin(t = 10), size = 10, family = "Palatino"),
        axis.title.y = element_text(margin = margin(r = 3), size = 10, family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.position = 'none',
        plot.tag = element_markdown(family = "Palatino", size = 7),
        plot.tag.position = c(0.425,0.95),
        plot.margin = unit(c(0, 0.5, 0.25, 0.5), "cm")) +
  labs(x = "Mean Temperature (<sup>o</sup>C)",
       y = "Relative Ear Length",
       tag = Ear_VertNet_Adult_M_temp)


VertNet <- cowplot::plot_grid(Berg_VertNet_temp, Tail_VertNet_temp, Ear_VertNet_temp, ncol = 1, nrow = 3, align = 'v',
                              labels = c('A)','C)','E)'), label_fontfamily = "Palatino", label_size = 12,
                              label_x = c(0.05, 0.05, 0.05), label_y = c(0.915, 1, 1.05), hjust = 0)

AdultMales <- cowplot::plot_grid(Berg_Adult_Male_VertNet_temp, Tail_Adult_Male_VertNet_temp, Ear_Adult_Male_VertNet_temp, ncol = 1, nrow = 3, align = 'v',
                                 labels = c('B)','D)','F)'), label_fontfamily = "Palatino", label_size = 12,
                                 label_x = c(0.05, 0.05, 0.05), label_y = c(0.915, 1, 1.05), hjust = 0)



cowplot::plot_grid(VertNet, AdultMales, ncol = 2, nrow = 1)

#ggsave("results/figures/VertNet_meantemp.tiff", height = 7, width = 6, compression = "lzw")
ggsave("results/figures/VertNet_meantemp.pdf", height = 7, width = 6)
