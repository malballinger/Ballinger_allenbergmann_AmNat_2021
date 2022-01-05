#!/usr/bin/env Rscript --vanilla

##############################################################

# Author: Mallory A. Ballinger

# This script plots body mass and relative extremity length from wild-caught
# house mice collected across North and South America. Data are from VertNet.org
# and were cleaned in ./clean_VertNetMetadata.R.
# This script generates Figure 1 in Ballinger_AmNat_2021.


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
library(effectsize)

set.seed(19910118) # so that jitter plots stay in same jittered positions


##############################################################
# Import data
##############################################################

VertNetMetadata <- read_csv(here("data/processed/VertNetMetadata_Mus_2021-03-18.csv")) %>%
  select(-1) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(Sex = fct_relevel(Sex, "male", "female")) # puts males before females


##############################################################
# Apply filtering and calculate relative lengths
##############################################################

# Based on outlier tests (see ./model_VertNetMetadata_relative.R), any tail length
# less than 20 and greater than 120 are extreme outliers
VertNet_filtered <- VertNetMetadata %>%
  mutate(Tail_Length_mm = ifelse(Tail_Length_mm < 20, NA, Tail_Length_mm),
         Tail_Length_mm = ifelse(Tail_Length_mm > 120, NA, Tail_Length_mm)) %>%
  mutate(RelativeTail = (Tail_Length_mm) / (Body_Weight_g))


# Based on outlier tests (see ./model_VertNetMetadata_relative.R), any ear length
# greater than 30 is extreme outliers
VertNet_filtered_2 <- VertNetMetadata %>%
  mutate(Ear_Length_mm = ifelse(Ear_Length_mm > 30, NA, Ear_Length_mm)) %>%
  mutate(RelativeEar = (Ear_Length_mm) / (Body_Weight_g))


##############################################################
# Are results sex-specific and do females show more variation than males?
##############################################################

# Broad evidence for Bergmann's rule and Allen's rule, after accounting for sex

# lm.BW <- lm(Body_Weight_g ~ Sex + Absolute_Latitude, data = VertNetMetadata)
# summary(lm.BW)
# lm.BW.Anova <- car::Anova(lm.BW)
# effect_size_BW <- omega_squared(lm.BW)
# 
# lm.RelTL <- lm(RelativeTail ~ Absolute_Latitude + Sex, data = VertNet_filtered)
# summary(lm.RelTL)
# lm.TL.Anova <- car::Anova(lm.RelTL)
# effect_size_TL <- omega_squared(lm.RelTL)
# 
# lm.RelEL <- lm(RelativeEar ~ Absolute_Latitude + Sex, data = VertNet_filtered_2)
# summary(lm.RelEL)
# lm.EL.Anova <- car::Anova(lm.RelEL)
# effect_size_EL <- omega_squared(lm.EL.Anova)


# Variance between sexes

# var.BM.model <- lm(Body_Weight_g ~ Sex, data = VertNetMetadata)
#check_model(var.BM.model) # close to normality
# leveneTest(Body_Weight_g ~ Sex, data = VertNetMetadata)
# leveneTest(RelativeTail ~ Sex, data = VertNet_filtered)
# leveneTest(RelativeEar ~ Sex, data = VertNet_filtered_2)
# boxplot(Body_Weight_g ~ Sex, data = VertNetMetadata)


##############################################################
# Get values from correlation analyses
##############################################################

# Refer to ./model_VertNetMetadata_relative.R for model comparisons and correlation analyses

## Bergmann's rule

Male_Bergmann_VertNet <- VertNetMetadata %>%
  filter(Sex == "male")
#check_model(lm(Body_Weight_g ~ Absolute_Latitude, data = Male_Bergmann_VertNet)) # looks good

Male_Bergmann_VertNet_N <- comma(length(which(!is.na(Male_Bergmann_VertNet$Body_Weight_g))))

cor.Berg.Male.VertNet <- cor.test(x = Male_Bergmann_VertNet$Absolute_Latitude,
                                  y = Male_Bergmann_VertNet$Body_Weight_g,
                                  method = 'spearman', exact = FALSE,
                                  adjust="fdr", alpha=0.5)

cor.Berg.Male.VertNet_corr <- signif(as.double(cor.Berg.Male.VertNet$estimate), digits = 2)
cor.Berg.Male.VertNet_pval <- signif(as.double(cor.Berg.Male.VertNet$p.value), digits = 2)


Female_Bergmann_VertNet <- VertNetMetadata %>%
  filter(Sex == "female")
#check_model(lm(Body_Weight_g ~ Absolute_Latitude, data = Female_Bergmann_VertNet)) # looks good

Female_Bergmann_VertNet_N <- comma(length(which(!is.na(Female_Bergmann_VertNet$Body_Weight_g))))

cor.Berg.Female.VertNet <- cor.test(x = Female_Bergmann_VertNet$Absolute_Latitude,
                                    y = Female_Bergmann_VertNet$Body_Weight_g,
                                    method = 'spearman', exact = FALSE,
                                    adjust="fdr", alpha=0.5)
#report(cor.Berg.Female.VertNet)

cor.Berg.Female.VertNet_corr <- signif(as.double(cor.Berg.Female.VertNet$estimate), digits = 2)
cor.Berg.Female.VertNet_pval <- signif(as.double(cor.Berg.Female.VertNet$p.value), digits = 2)


Male_Bergmann_Adult_VertNet <- Male_Bergmann_VertNet %>%
  filter(Lifestage == "adult")
Male_Bergmann_Adult_VertNet_N <- comma(length(which(!is.na(Male_Bergmann_Adult_VertNet$Body_Weight_g))))

cor.Berg.Male.Adult.VertNet <- cor.test(x = Male_Bergmann_Adult_VertNet$Absolute_Latitude,
                                        y = Male_Bergmann_Adult_VertNet$Body_Weight_g,
                                        method = 'spearman', exact = FALSE,
                                        adjust="fdr", alpha=0.5)
#report(cor.Berg.Male.Adult.VertNet)

cor.Berg.Male.Adult.VertNet_corr <- signif(as.double(cor.Berg.Male.Adult.VertNet$estimate), digits = 2)
cor.Berg.Male.Adult.VertNet_pval <- signif(as.double(cor.Berg.Male.Adult.VertNet$p.value), digits = 3)


## Allen's rule - TAIL
Male_Tail_VertNet <- VertNet_filtered %>%
  filter(Sex == "male")
#check_model(lm(RelativeTail ~ Absolute_Latitude, data = Male_Tail_VertNet)) # looks good

Male_Tail_VertNet_N <- comma(length(which(!is.na(Male_Tail_VertNet$RelativeTail))))

cor.Tail.Male.VertNet <- cor.test(x = Male_Tail_VertNet$Absolute_Latitude,
                                  y = Male_Tail_VertNet$RelativeTail,
                                  method = 'spearman', exact = FALSE,
                                  adjust="fdr", alpha=0.5)

cor.Tail.Male.VertNet_corr <- signif(as.double(cor.Tail.Male.VertNet$estimate), digits = 2)
cor.Tail.Male.VertNet_pval <- signif(as.double(cor.Tail.Male.VertNet$p.value), digits = 3)


Female_Tail_VertNet <- VertNet_filtered %>%
  filter(Sex == "female")
Female_Tail_VertNet_N <- comma(length(which(!is.na(Female_Tail_VertNet$RelativeTail))))

cor.Tail.Female.VertNet <- cor.test(x = Female_Tail_VertNet$Absolute_Latitude,
                                    y = Female_Tail_VertNet$RelativeTail,
                                    method = 'spearman', exact = FALSE,
                                    adjust="fdr", alpha=0.5)
#report(cor.Tail.Female.VertNet)

cor.Tail.Female.VertNet_corr <- signif(as.double(cor.Tail.Female.VertNet$estimate), digits = 2)
cor.Tail.Female.VertNet_pval <- signif(as.double(cor.Tail.Female.VertNet$p.value), digits = 2)


Male_Tail_Adult_VertNet <- Male_Tail_VertNet %>%
  filter(Lifestage == "adult")
Male_Tail_Adult_VertNet_N <- comma(length(which(!is.na(Male_Tail_Adult_VertNet$RelativeTail))))

cor.Tail.Male.Adult.VertNet <- cor.test(x = Male_Tail_Adult_VertNet$Absolute_Latitude,
                                        y = Male_Tail_Adult_VertNet$RelativeTail,
                                        method = 'spearman', exact = FALSE,
                                        adjust="fdr", alpha=0.5)
#report(cor.Tail.Male.Adult.VertNet)

cor.Tail.Male.Adult.VertNet_corr <- signif(as.double(cor.Tail.Male.Adult.VertNet$estimate), digits = 2)
cor.Tail.Male.Adult.VertNet_pval <- signif(as.double(cor.Tail.Male.Adult.VertNet$p.value), digits = 2)


## Allen's rule - EAR
Male_Ear_VertNet <- VertNet_filtered_2 %>%
  filter(Sex == "male")
Male_Ear_VertNet_N <- comma(length(which(!is.na(Male_Ear_VertNet$RelativeEar))))

cor.Ear.Male.VertNet <- cor.test(x = Male_Ear_VertNet$Absolute_Latitude,
                                 y = Male_Ear_VertNet$RelativeEar,
                                 method = 'spearman', exact = FALSE,
                                 adjust="fdr", alpha=0.5)
#report(cor.Ear.Male.VertNet)

cor.Ear.Male.VertNet_corr <- signif(as.double(cor.Ear.Male.VertNet$estimate), digits = 2)
cor.Ear.Male.VertNet_pval <- signif(as.double(cor.Ear.Male.VertNet$p.value), digits = 2)


Female_Ear_VertNet <- VertNet_filtered_2 %>%
  filter(Sex == "female")
Female_Ear_VertNet_N <- comma(length(which(!is.na(Female_Ear_VertNet$RelativeEar))))

cor.Ear.Female.VertNet <- cor.test(x = Female_Ear_VertNet$Absolute_Latitude,
                                   y = Female_Ear_VertNet$RelativeEar,
                                   method = 'spearman', exact = FALSE,
                                   adjust="fdr", alpha=0.5)
#report(cor.Ear.Female.VertNet)

cor.Ear.Female.VertNet_corr <- signif(as.double(cor.Ear.Female.VertNet$estimate), digits = 2)
cor.Ear.Female.VertNet_pval <- signif(as.double(cor.Ear.Female.VertNet$p.value), digits = 2)


Male_Ear_Adult_VertNet <- Male_Ear_VertNet %>%
  filter(Lifestage == "adult")
Male_Ear_Adult_VertNet_N <- comma(length(which(!is.na(Male_Ear_Adult_VertNet$RelativeEar))))

cor.Ear.Male.Adult.VertNet <- cor.test(x = Male_Ear_Adult_VertNet$Absolute_Latitude,
                                       y = Male_Ear_Adult_VertNet$RelativeEar,
                                       method = 'spearman', exact = FALSE,
                                       adjust="fdr", alpha=0.5)
#report(cor.Ear.Male.Adult.VertNet)

cor.Ear.Male.Adult.VertNet_corr <- signif(as.double(cor.Ear.Male.Adult.VertNet$estimate), digits = 2)
cor.Ear.Male.Adult.VertNet_pval <- signif(as.double(cor.Ear.Male.Adult.VertNet$p.value), digits = 2)


##############################################################
# Save results for each sex
##############################################################

Berg_VertNet_M <- glue("males (rho = {cor.Berg.Male.VertNet_corr},  \\
                      *P* = {cor.Berg.Male.VertNet_pval}, \\
                       *n* = {Male_Bergmann_VertNet_N})")
Berg_VertNet_F <- glue("females (rho = {cor.Berg.Female.VertNet_corr}, \\
                       *P* = {cor.Berg.Female.VertNet_pval}, \\
                       *n* = {Female_Bergmann_VertNet_N})")
Berg_VertNet_Adult_M <- glue("rho = {cor.Berg.Male.Adult.VertNet_corr}, \\
                             *P* < 0.0001, \\
                             *n* = {Male_Bergmann_Adult_VertNet_N}")

Tail_VertNet_M <- glue ("males (rho = {cor.Tail.Male.VertNet_corr}, \\
                       *P* < 0.0001, \\
                       *n* = {Male_Tail_VertNet_N})")
Tail_VertNet_F <- glue("females (rho = {cor.Tail.Female.VertNet_corr}, \\
                       *P* = {cor.Tail.Female.VertNet_pval}, \\
                       *n* = {Female_Tail_VertNet_N})")
Tail_VertNet_Adult_M <- glue("rho = {cor.Tail.Male.Adult.VertNet_corr}, \\
                             *P* < 0.0001, \\
                             *n* = {Male_Tail_Adult_VertNet_N}")

Ear_VertNet_M <- glue("males (rho = {cor.Ear.Male.VertNet_corr}, \\
                      *P* = {cor.Ear.Male.VertNet_pval}, \\
                      *n* = {Male_Ear_VertNet_N})")
Ear_VertNet_F <- glue("females (rho = {cor.Ear.Female.VertNet_corr}, \\
                      *P* = {cor.Ear.Female.VertNet_pval}, \\
                      *n* = {Female_Ear_VertNet_N})")
Ear_VertNet_Adult_M <- glue("rho = {cor.Ear.Male.Adult.VertNet_corr}, \\
                            *P* < 0.0001, \\
                            *n* = {Male_Ear_Adult_VertNet_N}")


##############################################################
# Combine all 6 plots for manuscript
##############################################################

Berg_VertNet <-
  ggplot(data = VertNetMetadata, aes(x = Absolute_Latitude, y = Body_Weight_g)) +
  geom_jitter(aes(fill = Sex), size = 1.85, stroke = 0.25, height = 0, width = 0.2, shape = 21, alpha = 0.9, color = "black") +
  geom_smooth(aes(linetype = Sex, color = Sex), method = "lm", se = FALSE, size = 0.5) +
  scale_color_manual(labels=c(Berg_VertNet_M, Berg_VertNet_F),
                     values=c("black", "darkgray")) +
  scale_fill_manual(labels=c(Berg_VertNet_M, Berg_VertNet_F),
                    values=c("black", "white")) +
  scale_linetype_manual(labels=c(Berg_VertNet_M, Berg_VertNet_F),
                        values = c("solid", "solid")) +
  scale_x_continuous(breaks = seq(from=0, to=65, by=10),
                     labels = seq(from=0, to=65, by=10),
                     limits = c(0,65)) +
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


Tail_VertNet <-
  ggplot(data = VertNet_filtered, aes(x = Absolute_Latitude, y = RelativeTail)) +
  geom_jitter(aes(fill = Sex), size = 1.85, stroke = 0.25, height = 0, width = 0.2, shape = 21, alpha = 0.9, color = "black") +
  geom_smooth(aes(linetype = Sex, color = Sex), method = "lm", se = FALSE, size = 0.5) +
  scale_color_manual(labels=c(Tail_VertNet_M, Tail_VertNet_F),
                     values=c("black", "darkgray")) +
  scale_fill_manual(labels=c(Tail_VertNet_M, Tail_VertNet_F),
                    values=c("black", "white")) +
  scale_linetype_manual(labels=c(Tail_VertNet_M, Tail_VertNet_F),
                        values = c("solid", "solid")) +
  scale_x_continuous(breaks = seq(from=0, to=65, by=10),
                     labels = seq(from=0, to=65, by=10),
                     limits = c(0,65)) +
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
        legend.position = c(0.457, 0.95),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key.size = unit(0.25, "lines"),
        legend.text = element_markdown(size=6.5, family = "Palatino"),
        legend.key = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.25, 0.5, 0.25, 0.5), "cm")) +
  labs(x = NULL,
       y = "Relative Tail Length")


Ear_VertNet <-
  ggplot(data = VertNet_filtered_2, aes(x = Absolute_Latitude, y = RelativeEar)) +
  geom_jitter(aes(fill = Sex), size = 1.85, stroke = 0.25, height = 0, width = 0.2, shape = 21, alpha = 0.9, color = "black") +
  geom_smooth(aes(linetype = Sex, color = Sex), method = "lm", se = FALSE, size = 0.5) +
  scale_color_manual(labels=c(Ear_VertNet_M, Ear_VertNet_F),
                     values=c("black", "darkgray")) +
  scale_fill_manual(labels=c(Ear_VertNet_M, Ear_VertNet_F),
                    values=c("black", "white")) +
  scale_linetype_manual(labels=c(Ear_VertNet_M, Ear_VertNet_F),
                        values = c("solid", "solid")) +
  scale_x_continuous(breaks = seq(from=0, to=65, by=10),
                     labels = seq(from=0, to=65, by=10),
                     limits = c(0,65)) +
  scale_y_continuous(breaks = seq(from=0, to=3.5, by=1),
                     labels = seq(from=0, to=3.5, by=1),
                     limits = c(NA,3.2)) +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10), size = 10, family = "Palatino"),
        axis.title.y = element_text(margin = margin(r = 0), size = 10, family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.title = element_blank(),
        legend.position = c(0.45, 0.95),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key.size = unit(0.25, "lines"),
        legend.text = element_markdown(size=6.5, family = "Palatino"),
        legend.key = element_rect(fill = "transparent"),
        plot.margin = unit(c(0, 0.5, 0.25, 0.5), "cm")) +
  labs(x = "Degrees from the Equator",
       y = "Relative Ear Length")


##############################################################
# Adult males only from VertNet
##############################################################

Berg_Adult_Male_VertNet <-
  ggplot(data = Male_Bergmann_Adult_VertNet, aes(x = Absolute_Latitude, y = Body_Weight_g, color = "black", fill = "black")) +
  geom_jitter(size = 2.3, stroke = 0.25, height = 0, width = 0.2, shape = 21, alpha = 0.75, show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE, size = 0.5, show.legend = FALSE) +
  scale_color_manual(labels=c(Tail_VertNet_Adult_M),
                     values=c("white")) +
  scale_fill_manual(labels=c(Tail_VertNet_Adult_M),
                    values=c("black")) +
  scale_x_continuous(breaks = seq(from=0, to=65, by=10),
                     labels = seq(from=0, to=65, by=10),
                     limits = c(0,65)) +
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
       tag = Berg_VertNet_Adult_M,
       title = "VertNet Adult Males")


Tail_Adult_Male_VertNet <-
  ggplot(data = Male_Tail_Adult_VertNet, aes(x = Absolute_Latitude, y = RelativeTail, color = "black", fill = "black")) +
  geom_jitter(size = 2.3, stroke = 0.25, height = 0, width = 0.2, shape = 21, alpha = 0.75, show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE, size = 0.5, show.legend = FALSE) +
  scale_color_manual(labels=c(Tail_VertNet_Adult_M),
                     values=c("white")) +
  scale_fill_manual(labels=c(Tail_VertNet_Adult_M),
                    values=c("black")) +
  scale_x_continuous(breaks = seq(from=0, to=65, by=10),
                     labels = seq(from=0, to=65, by=10),
                     limits = c(0,65)) +
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
        plot.tag.position = c(0.44,0.95),
        plot.margin = unit(c(0.25, 0.5, 0.25, 0.5), "cm")) +
  labs(x = NULL,
       y = "Relative Tail Length",
       tag = Tail_VertNet_Adult_M)


Ear_Adult_Male_VertNet <-
  ggplot(data = Male_Ear_Adult_VertNet, aes(x = Absolute_Latitude, y = RelativeEar, color = "black", fill = "black")) +
  geom_jitter(size = 2.3, stroke = 0.25, height = 0, width = 0.2, shape = 21, alpha = 0.75, show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE, size = 0.5, show.legend = FALSE) +
  scale_color_manual(labels=c(Ear_VertNet_Adult_M),
                     values=c("white")) +
  scale_fill_manual(labels=c(Ear_VertNet_Adult_M),
                    values=c("black")) +
  scale_x_continuous(breaks = seq(from=0, to=65, by=10),
                     labels = seq(from=0, to=65, by=10),
                     limits = c(0,65)) +
  scale_y_continuous(breaks = seq(from=0.4, to=1.7, by=0.5),
                     labels = seq(from=0.4, to=1.7, by=0.5),
                     limits = c(0.4,1.7)) +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10), size = 10, family = "Palatino"),
        axis.title.y = element_text(margin = margin(r = 3), size = 10, family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.position = 'none',
        plot.tag = element_markdown(family = "Palatino", size = 7),
        plot.tag.position = c(0.445,0.95),
        plot.margin = unit(c(0, 0.5, 0.25, 0.5), "cm")) +
  labs(x = "Degrees from the Equator",
       y = "Relative Ear Length",
       tag = Ear_VertNet_Adult_M)


VertNet <- cowplot::plot_grid(Berg_VertNet, Tail_VertNet, Ear_VertNet, ncol = 1, nrow = 3, align = 'v',
                              labels = c('A)','C)','E)'), label_fontfamily = "Palatino", label_size = 12,
                              label_x = c(0.05, 0.05, 0.05), label_y = c(0.915, 1, 1.05), hjust = 0)

AdultMales <- cowplot::plot_grid(Berg_Adult_Male_VertNet, Tail_Adult_Male_VertNet, Ear_Adult_Male_VertNet, ncol = 1, nrow = 3, align = 'v',
                                 labels = c('B)','D)','F)'), label_fontfamily = "Palatino", label_size = 12,
                                 label_x = c(0.05, 0.05, 0.05), label_y = c(0.915, 1, 1.05), hjust = 0)



cowplot::plot_grid(VertNet, AdultMales, ncol = 2, nrow = 1)

#ggsave("results/figures/VertNet_relative.tiff", height = 7, width = 6, compression = "lzw")
ggsave("results/figures/VertNet_relative.pdf", height = 7, width = 6)
