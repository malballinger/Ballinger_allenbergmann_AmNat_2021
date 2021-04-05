#!/usr/bin/env Rscript --vanilla

##############################################################
# Author: Mallory A. Ballinger
# Script first created: 23-Feb-2021
# Script last updated:  03-Apr-2021


# This script plots body mass and tail length from wild-caught house mice,
# collected across North and South America. Data are from VertNet.org and from
# the Nachman lab project dowloaded from Arctos.org.
# This script generates Figure 1 in Ballinger_et_al_2021_AmNat.


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

##############################################################
# Import data
##############################################################

VertNetMetadata <- read_csv(here("data/processed/VertNetMetadata_Mus_2021-03-18.csv")) %>%
  mutate(Ear_Length_mm = as.numeric(Ear_Length_mm),
         Hindfoot_Length_mm = as.numeric(Hindfoot_Length_mm),
         Tail_Length_mm = as.numeric(Tail_Length_mm),
         Body_Length_mm = as.numeric(Body_Length_mm),
         Body_Weight_g = as.numeric(Body_Weight_g)) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(Sex = fct_relevel(Sex, "male", "female")) # puts males before females


NachmanTransectsMetadata <- read_csv(here("data/processed/EnvAdapProj_Nachman_Arctos_transects_2021-03-15.csv")) %>%
  mutate(Ear_Length_mm = as.numeric(Ear_Length_mm),
         Hindfoot_Length_mm = as.numeric(Hindfoot_Length_mm),
         Tail_Length_mm = as.numeric(Tail_Length_mm),
         Body_Length_mm = as.numeric(Body_Length_mm),
         Body_Weight_g = as.numeric(Body_Weight_g)) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(Sex = fct_relevel(Sex, "male", "female")) # puts males before females


# get sample size of each column
#colSums(!is.na(VertNetMetadata))
#colSums(!is.na(NachmanTransectsMetadata))





##############################################################
# Apply filtering and calculate residuals
##############################################################

# VertNet dataset

# Based on outlier tests (see 'code/model_VertNetMetadata.R'), any tail length
# less than 20 and greater than 120 are extreme outliers
VertNet_filtered <- VertNetMetadata %>%
  mutate(Tail_Length_mm = ifelse(Tail_Length_mm < 20, NA, Tail_Length_mm),
         Tail_Length_mm = ifelse(Tail_Length_mm > 120, NA, Tail_Length_mm))

# save TL x BW residuals
residsTLBW <- lm(Tail_Length_mm ~ Body_Weight_g, data = VertNet_filtered,
                 na.action = na.exclude)
VertNet_filtered$Resids_TLBW <- resid(residsTLBW)


# Based on outlier tests (see 'code/model_VertNetMetadata.R'), any ear length
# greater than 30 is extreme outliers
VertNet_filtered_2 <- VertNetMetadata %>%
  mutate(Ear_Length_mm = ifelse(Ear_Length_mm > 30, NA, Ear_Length_mm))

# save EL x BW residuals
residsELBW <- lm(Ear_Length_mm ~ Body_Weight_g, data = VertNet_filtered_2,
                 na.action = na.exclude)
VertNet_filtered_2$Resids_ELBW <- resid(residsELBW)



# NachmanArctos dataset

# Based on outlier tests (see 'code/model_NachmanTransects.R'), any tail length
# less than 25 is an extreme outlier
NachmanTransects_filtered <- NachmanTransectsMetadata %>%
  mutate(Tail_Length_mm = ifelse(Tail_Length_mm < 25, NA, Tail_Length_mm))

# save TL x BW residuals
residsTLBW <- lm(Tail_Length_mm ~ Body_Weight_g, data = NachmanTransects_filtered,
                 na.action = na.exclude)
NachmanTransects_filtered$Resids_TLBW <- resid(residsTLBW)


# save EL x BW residuals
residsELBW <- lm(Ear_Length_mm ~ Body_Weight_g, data = NachmanTransectsMetadata,
                 na.action = na.exclude)
NachmanTransectsMetadata$Resids_ELBW <- resid(residsELBW)





##############################################################
# Get values from correlation analyses
##############################################################
# Refer to (code/model_VertNetMetadata.R) and (code/model_NachmanTransects.R)
# for model comparisons and correlation analyses

# VertNet Metadata
## Bergmann's rule
Male_Bergmann_VertNet <- VertNetMetadata %>%
  filter(Sex == "male")
Male_Bergmann_VertNet_N <- comma(length(which(!is.na(Male_Bergmann_VertNet$Body_Weight_g))))

cor.Berg.Male.VertNet <- cor.test(x = Male_Bergmann_VertNet$Absolute_Latitude,
                                  y = Male_Bergmann_VertNet$Body_Weight_g,
                                  method = 'spearman', exact = FALSE,
                                  adjust="fdr", alpha=0.5)

cor.Berg.Male.VertNet_corr <- round(as.double(cor.Berg.Male.VertNet$estimate),3)
cor.Berg.Male.VertNet_pval <- round(as.double(cor.Berg.Male.VertNet$p.value),3)


Female_Bergmann_VertNet <- VertNetMetadata %>%
  filter(Sex == "female")
Female_Bergmann_VertNet_N <- comma(length(which(!is.na(Female_Bergmann_VertNet$Body_Weight_g))))

cor.Berg.Female.VertNet <- cor.test(x = Female_Bergmann_VertNet$Absolute_Latitude,
                                    y = Female_Bergmann_VertNet$Body_Weight_g,
                                    method = 'spearman', exact = FALSE,
                                    adjust="fdr", alpha=0.5)

cor.Berg.Female.VertNet_corr <- round(as.double(cor.Berg.Female.VertNet$estimate),3)
cor.Berg.Female.VertNet_pval <- round(as.double(cor.Berg.Female.VertNet$p.value),2)


## Allen's rule - TAIL
Male_Tail_VertNet <- VertNet_filtered %>%
  filter(Sex == "male")
Male_Tail_VertNet_N <- comma(length(which(!is.na(Male_Tail_VertNet$Resids_TLBW))))

cor.Tail.Male.VertNet <- cor.test(x = Male_Tail_VertNet$Absolute_Latitude,
                                  y = Male_Tail_VertNet$Resids_TLBW,
                                  method = 'spearman', exact = FALSE,
                                  adjust="fdr", alpha=0.5)

cor.Tail.Male.VertNet_corr <- round(as.double(cor.Tail.Male.VertNet$estimate),2)
cor.Tail.Male.VertNet_pval <- round(as.double(cor.Tail.Male.VertNet$p.value),3)


Female_Tail_VertNet <- VertNet_filtered %>%
  filter(Sex == "female")
Female_Tail_VertNet_N <- comma(length(which(!is.na(Female_Tail_VertNet$Resids_TLBW))))

cor.Tail.Female.VertNet <- cor.test(x = Female_Tail_VertNet$Absolute_Latitude,
                                    y = Female_Tail_VertNet$Resids_TLBW,
                                    method = 'spearman', exact = FALSE,
                                    adjust="fdr", alpha=0.5)

cor.Tail.Female.VertNet_corr <- round(as.double(cor.Tail.Female.VertNet$estimate),2)
cor.Tail.Female.VertNet_pval <- round(as.double(cor.Tail.Female.VertNet$p.value),3)


## Allen's rule - EAR
Male_Ear_VertNet <- VertNet_filtered_2 %>%
  filter(Sex == "male")
Male_Ear_VertNet_N <- comma(length(which(!is.na(Male_Ear_VertNet$Resids_ELBW))))

cor.Ear.Male.VertNet <- cor.test(x = Male_Ear_VertNet$Absolute_Latitude,
                                 y = Male_Ear_VertNet$Resids_ELBW,
                                 method = 'spearman', exact = FALSE,
                                 adjust="fdr", alpha=0.5)

cor.Ear.Male.VertNet_corr <- round(as.double(cor.Ear.Male.VertNet$estimate),2)
cor.Ear.Male.VertNet_pval <- round(as.double(cor.Ear.Male.VertNet$p.value),3)


Female_Ear_VertNet <- VertNet_filtered_2 %>%
  filter(Sex == "female")
Female_Ear_VertNet_N <- comma(length(which(!is.na(Female_Ear_VertNet$Resids_ELBW))))

cor.Ear.Female.VertNet <- cor.test(x = Female_Ear_VertNet$Absolute_Latitude,
                                   y = Female_Ear_VertNet$Resids_ELBW,
                                   method = 'spearman', exact = FALSE,
                                   adjust="fdr", alpha=0.5)

cor.Ear.Female.VertNet_corr <- round(as.double(cor.Ear.Female.VertNet$estimate),3)
cor.Ear.Female.VertNet_pval <- round(as.double(cor.Ear.Female.VertNet$p.value),3)



# NachmanArctos Transects
## Bergmann's rule
Male_Bergmann_Arctos <- NachmanTransectsMetadata %>%
  filter(Sex == "male")
Male_Bergmann_Arctos_N <- comma(length(which(!is.na(Male_Bergmann_Arctos$Body_Weight_g))))

cor.Berg.Male.Arctos <- cor.test(x = Male_Bergmann_Arctos$Absolute_Latitude,
                                 y = Male_Bergmann_Arctos$Body_Weight_g,
                                 method = 'pearson', exact = FALSE,
                                 adjust="fdr", alpha=0.5)

cor.Berg.Male.Arctos_corr <- round(as.double(cor.Berg.Male.Arctos$estimate),2)
cor.Berg.Male.Arctos_pval <- round(as.double(cor.Berg.Male.Arctos$p.value),2)


Female_Bergmann_Arctos <- NachmanTransectsMetadata %>%
  filter(Sex == "female")
Female_Bergmann_Arctos_N <- comma(length(which(!is.na(Female_Bergmann_Arctos$Body_Weight_g))))

cor.Berg.Female.Arctos <- cor.test(x = Female_Bergmann_Arctos$Absolute_Latitude,
                                   y = Female_Bergmann_Arctos$Body_Weight_g,
                                   method = 'pearson', exact = FALSE,
                                   adjust="fdr", alpha=0.5)

cor.Berg.Female.Arctos_corr <- round(as.double(cor.Berg.Female.Arctos$estimate),2)
cor.Berg.Female.Arctos_pval <- round(as.double(cor.Berg.Female.Arctos$p.value),2)


## Allen's rule - TAIL
Male_Tail_Arctos <- NachmanTransects_filtered %>%
  filter(Sex == "male")
Male_Tail_Arctos_N <- comma(length(which(!is.na(Male_Tail_Arctos$Resids_TLBW))))

cor.Tail.Male.Arctos <- cor.test(x = Male_Tail_Arctos$Absolute_Latitude,
                                 y = Male_Tail_Arctos$Resids_TLBW,
                                 method = 'spearman', exact = FALSE,
                                 adjust="fdr", alpha=0.5)

cor.Tail.Male.Arctos_corr <- round(as.double(cor.Tail.Male.Arctos$estimate),3)
cor.Tail.Male.Arctos_pval <- round(as.double(cor.Tail.Male.Arctos$p.value),2)


Female_Tail_Arctos <- NachmanTransects_filtered %>%
  filter(Sex == "female")
Female_Tail_Arctos_N <- comma(length(which(!is.na(Female_Tail_Arctos$Resids_TLBW))))

cor.Tail.Female.Arctos <- cor.test(x = Female_Tail_Arctos$Absolute_Latitude,
                                   y = Female_Tail_Arctos$Resids_TLBW,
                                   method = 'spearman', exact = FALSE,
                                   adjust="fdr", alpha=0.5)

cor.Tail.Female.Arctos_corr <- round(as.double(cor.Tail.Female.Arctos$estimate),2)
cor.Tail.Female.Arctos_pval <- round(as.double(cor.Tail.Female.Arctos$p.value),2)


## Allen's rule - EAR
Male_Ear_Arctos <- NachmanTransectsMetadata %>%
  filter(Sex == "male")
Male_Ear_Arctos_N <- comma(length(which(!is.na(Male_Ear_Arctos$Resids_ELBW))))

cor.Ear.Male.Arctos <- cor.test(x = Male_Ear_Arctos$Absolute_Latitude,
                                y = Male_Ear_Arctos$Resids_ELBW,
                                method = 'spearman', exact = FALSE,
                                adjust="fdr", alpha=0.5)

cor.Ear.Male.Arctos_corr <- round(as.double(cor.Ear.Male.Arctos$estimate),3)
cor.Ear.Male.Arctos_pval <- round(as.double(cor.Ear.Male.Arctos$p.value),2)


Female_Ear_Arctos <- NachmanTransectsMetadata %>%
  filter(Sex == "female")
Female_Ear_Arctos_N <- comma(length(which(!is.na(Female_Ear_Arctos$Resids_ELBW))))

cor.Ear.Female.Arctos <- cor.test(x = Female_Ear_Arctos$Absolute_Latitude,
                                  y = Female_Ear_Arctos$Resids_ELBW,
                                  method = 'spearman', exact = FALSE,
                                  adjust="fdr", alpha=0.5)

cor.Ear.Female.Arctos_corr <- round(as.double(cor.Ear.Female.Arctos$estimate),2)
cor.Ear.Female.Arctos_pval <- round(as.double(cor.Ear.Female.Arctos$p.value),3)





##############################################################
# Save results for each sex
##############################################################

Berg_VertNet_M <- glue("males (rho = {cor.Berg.Male.VertNet_corr},  \\
                      *P* = {cor.Berg.Male.VertNet_pval}, \\
                      *n* = {Male_Bergmann_VertNet_N})")
Berg_VertNet_F <- glue("females (rho = {cor.Berg.Female.VertNet_corr}, \\
                      *P* = {cor.Berg.Female.VertNet_pval}, \\
                      *n* = {Female_Bergmann_VertNet_N})")

Tail_VertNet_M <- glue ("males (rho = {cor.Tail.Male.VertNet_corr}, \\
                       *P* < 0.0001, \\
                       *n* = {Male_Tail_VertNet_N})")
Tail_VertNet_F <- glue("females (rho = {cor.Tail.Female.VertNet_corr}, \\
                       *P* < 0.0001, \\
                       *n* = {Female_Tail_VertNet_N})")

Ear_VertNet_M <- glue("males (rho = {cor.Ear.Male.VertNet_corr}, \\
                      *P* < 0.001, \\
                      *n* = {Male_Ear_VertNet_N})")
Ear_VertNet_F <- glue("females (rho = {cor.Ear.Female.VertNet_corr}, \\
                      *P* = {cor.Ear.Female.VertNet_pval}, \\
                      *n* = {Female_Ear_VertNet_N})")


Berg_Arctos_M <- glue("males (cor = {cor.Berg.Male.Arctos_corr}, \\
                      *P* < 0.001, \\
                      *n* = {Male_Bergmann_Arctos_N})")
Berg_Arctos_F <- glue("females (cor = {cor.Berg.Female.Arctos_corr}, \\
                      *P* < 0.001, \\
                      *n* = {Female_Bergmann_Arctos_N})")

Tail_Arctos_M <- glue("males (rho = {cor.Tail.Male.Arctos_corr}, \\
                      *P* = {cor.Tail.Male.Arctos_pval}, \\
                      *n* = {Male_Tail_Arctos_N})")
Tail_Arctos_F <- glue("females (rho = {cor.Tail.Female.Arctos_corr}, \\
                      *P* = {cor.Tail.Female.Arctos_pval}, \\
                      *n* = {Female_Tail_Arctos_N})")

Ear_Arctos_M <- glue("males (rho = {cor.Ear.Male.Arctos_corr}, \\
                     *P* = {cor.Ear.Male.Arctos_pval}, \\
                     *n* = {Male_Ear_Arctos_N})")
Ear_Arctos_F <- glue("females (rho = {cor.Ear.Female.Arctos_corr}, \\
                     *P* = {cor.Ear.Female.Arctos_pval}, \\
                     *n* = {Female_Ear_Arctos_N})")




##############################################################
# Combine all 6 plots for publication
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
        axis.title.x = element_text(margin = margin(t = 0), size = 10, face = "bold", family = "Palatino"),
        axis.title.y = element_text(margin = margin(r = 0), size = 10, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.title = element_blank(),
        legend.position = c(0.43, 0.95),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key.size = unit(0.25, "lines"),
        legend.text = element_markdown(size=6.5, family = "Palatino"),
        legend.key = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.25, 0.5, 0, 0.5), "cm"), #top, right, bottom, left
        plot.title = element_text(size = 11, face = "bold.italic", hjust = 0.5, vjust = 0.1, family = "Palatino")) +
  labs(x = "",
       y = "Body Mass (g)",
       title = "VertNet Metadata")


Berg_Arctos <-
  ggplot(data = NachmanTransectsMetadata, aes(x = Absolute_Latitude, y = Body_Weight_g)) +
  geom_jitter(aes(fill = Sex), size = 1.85, stroke = 0.25, height = 0, width = 0.2, shape = 21, alpha = 0.9, color = "black") +
  geom_smooth(aes(linetype = Sex, color = Sex), method = "lm", se = FALSE, size = 0.5) +
  scale_color_manual(labels=c(Berg_Arctos_M, Berg_Arctos_F),
                     values=c("black", "darkgray")) +
  scale_fill_manual(labels=c(Berg_Arctos_M, Berg_Arctos_F),
                    values=c("black", "white")) +
  scale_linetype_manual(labels=c(Berg_Arctos_M, Berg_Arctos_F),
                        values = c("solid", "solid")) +
  scale_x_continuous(breaks = seq(from=0, to=55, by=10),
                     labels = seq(from=0, to=55, by=10),
                     limits = c(0,55)) +
  scale_y_continuous(breaks = seq(from=0, to=30, by=10),
                     labels = seq(from=0, to=30, by=10),
                     limits = c(0,33)) +
  theme_bw() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 0), size = 10, face = "bold", family = "Palatino"),
        axis.title.y = element_text(margin = margin(r = 0), size = 10, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.title = element_blank(),
        legend.position = c(0.415, 0.95),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key.size = unit(0.25, "lines"),
        legend.text = element_markdown(size=6.5, family = "Palatino"),
        legend.key = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.25, 0.5, 0, 0.5), "cm"),
        plot.title = element_text(size = 11, face = "bold.italic", hjust = 0.5, vjust = 0.1, family = "Palatino")) +
  labs(x = "",
       y = "Body Mass (g)",
       title = "Nachman Transects")

#Berg <- cowplot::plot_grid(Berg_VertNet, Berg_Arctos, labels = c('A','B'), ncol = 2, nrow = 1, label_fontfamily = "Palatino", label_size = 12, label_x = 0.05, hjust = 0)


Tail_VertNet <-
  ggplot(data = VertNet_filtered, aes(x = Absolute_Latitude, y = Resids_TLBW)) +
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
  scale_y_continuous(breaks = seq(from=-50, to=30, by=10),
                     labels = seq(from=-50, to=30, by=10),
                     limits = c(-50,40)) +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 0), size = 10, face = "bold", family = "Palatino"),
        axis.title.y = element_text(margin = margin(r = 3), size = 10, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.title = element_blank(),
        legend.position = c(0.44, 0.95),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key.size = unit(0.25, "lines"),
        legend.text = element_markdown(size=6.5, family = "Palatino"),
        legend.key = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.25, 0.5, 0.25, 0.5), "cm")) +
  labs(x = "",
       y = "Tail Length (resids)")


Tail_Arctos <-
  ggplot(data = NachmanTransects_filtered, aes(x = Absolute_Latitude, y = Resids_TLBW)) +
  geom_jitter(aes(fill = Sex), size = 1.85, stroke = 0.25, height = 0, width = 0.2, shape = 21, alpha = 0.9, color = "black") +
  geom_smooth(aes(linetype = Sex, color = Sex), method = "lm", se = FALSE, size = 0.5) +
  scale_color_manual(labels=c(Tail_Arctos_M, Tail_Arctos_F),
                     values=c("black", "darkgray")) +
  scale_fill_manual(labels=c(Tail_Arctos_M, Tail_Arctos_F),
                    values=c("black", "white")) +
  scale_linetype_manual(labels=c(Tail_Arctos_M, Tail_Arctos_F),
                        values = c("solid", "solid")) +
  scale_x_continuous(breaks = seq(from=0, to=55, by=10),
                     labels = seq(from=0, to=55, by=10),
                     limits = c(0,55)) +
  scale_y_continuous(breaks = seq(from=-30, to=20, by=10),
                     labels = seq(from=-30, to=20, by=10),
                     limits = c(-30,25)) +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 0), size = 10, face = "bold", family = "Palatino"),
        axis.title.y = element_text(margin = margin(r = 3), size = 10, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.title = element_blank(),
        legend.position = c(0.42, 0.95),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key.size = unit(0.25, "lines"),
        legend.text = element_markdown(size=6.5, family = "Palatino"),
        legend.key = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.25, 0.5, 0.25, 0.5), "cm")) +
  labs(x = "",
       y = "Tail Length (resids)")

#Tail <- cowplot::plot_grid(Tail_VertNet, Tail_Arctos, labels = c('C','D'), ncol = 2, nrow = 1, label_fontfamily = "Palatino", label_size = 12, label_x = 0.05, hjust = 0)


Ear_VertNet <-
  ggplot(data = VertNet_filtered_2, aes(x = Absolute_Latitude, y = Resids_ELBW)) +
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
  scale_y_continuous(breaks = seq(from=-10, to=10, by=5),
                     labels = seq(from=-10, to=10, by=5),
                     limits = c(-10,12)) +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10), size = 10, face = "bold", family = "Palatino"),
        axis.title.y = element_text(margin = margin(r = 0), size = 10, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.title = element_blank(),
        legend.position = c(0.44, 0.95),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key.size = unit(0.25, "lines"),
        legend.text = element_markdown(size=6.5, family = "Palatino"),
        legend.key = element_rect(fill = "transparent"),
        plot.margin = unit(c(0, 0.5, 0.25, 0.5), "cm")) +
  labs(x = "Degrees from the Equator",
       y = "Ear Length (resids)")


Ear_Arctos <-
  ggplot(data = NachmanTransectsMetadata, aes(x = Absolute_Latitude, y = Resids_ELBW)) +
  geom_jitter(aes(fill = Sex), size = 1.85, stroke = 0.25, height = 0, width = 0.2, shape = 21, alpha = 0.9, color = "black") +
  geom_smooth(aes(linetype = Sex, color = Sex), method = "lm", se = FALSE, size = 0.5) +
  scale_color_manual(labels=c(Ear_Arctos_M, Ear_Arctos_F),
                     values=c("black", "darkgray")) +
  scale_fill_manual(labels=c(Ear_Arctos_M, Ear_Arctos_F),
                    values=c("black", "white")) +
  scale_linetype_manual(labels=c(Ear_Arctos_M, Ear_Arctos_F),
                        values = c("solid", "solid")) +
  scale_x_continuous(breaks = seq(from=0, to=55, by=10),
                     labels = seq(from=0, to=55, by=10),
                     limits = c(0,55)) +
  scale_y_continuous(breaks = seq(from=-5, to=7, by=5),
                     labels = seq(from=-5, to=7, by=5),
                     limits = c(-5,7)) +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10), size = 10, face = "bold", family = "Palatino"),
        axis.title.y = element_text(margin = margin(r = 0), size = 10, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.title = element_blank(),
        legend.position = c(0.43, 0.95),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key.size = unit(0.25, "lines"),
        legend.text = element_markdown(size=6.5, family = "Palatino"),
        legend.key = element_rect(fill = "transparent"),
        plot.margin = unit(c(0, 0.5, 0.25, 0.5), "cm")) +
  labs(x = "Degrees from the Equator",
       y = "Ear Length (resids)")



VertNet <- cowplot::plot_grid(Berg_VertNet, Tail_VertNet, Ear_VertNet, ncol = 1, nrow = 3, align = 'v', labels = c('A)','C)','E)'), label_fontfamily = "Palatino", label_size = 12, label_x = 0.05, hjust = 0)

Arctos <- cowplot::plot_grid(Berg_Arctos, Tail_Arctos, Ear_Arctos, ncol = 1, nrow = 3, align = 'v', labels = c('B)','D)','F)'), label_fontfamily = "Palatino", label_size = 12, label_x = 0.05, hjust = 0)

cowplot::plot_grid(VertNet, Arctos, ncol = 2, nrow = 1)


ggsave("results/figures/VertNet_Arctos.tiff", height = 7, width = 6, compression = "lzw")
ggsave("results/figures/VertNet_Arctos.pdf", height = 7, width = 6)