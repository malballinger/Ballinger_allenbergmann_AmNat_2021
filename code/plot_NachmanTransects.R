#!/usr/bin/env Rscript --vanilla

##############################################################
# Author: Mallory A. Ballinger
# Script first created: 23-Feb-2021
# Script last updated:  15-Mar-2021


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

NachmanTransectsMetadata <- read_csv(here("data/processed/EnvAdapProj_Nachman_Arctos_transects_2021-03-15.csv")) %>%
  mutate(Ear_Length_mm = as.numeric(Ear_Length_mm),
         Hindfoot_Length_mm = as.numeric(Hindfoot_Length_mm),
         Tail_Length_mm = as.numeric(Tail_Length_mm),
         Body_Length_mm = as.numeric(Body_Length_mm),
         Body_Weight_g = as.numeric(Body_Weight_g)) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(Sex = fct_relevel(Sex, "male", "female")) # puts males before females

# get sample size of each column
colSums(!is.na(NachmanTransectsMetadata))





##############################################################
# Apply filtering and calculate residuals
##############################################################

# filtering and model testing were completed in 'code/model_NachmanTransects.R'.
# We are applying the same filtering, models, and statistical outputs here for
# plotting purposes. Please see 'modeling' script for details.


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
# Plot body weight and exremity length vs. abs lat
##############################################################

Berg_deets <- c("males (cor = 0.36, *p* < 0.001)",
                "females (cor = 0.33, *p* < 0.001)")

Tail_deets <- c("males (rho = -0.052, *p* = 0.60)",
                "females (rho = -0.11, *p* = 0.23)")

Ear_deets <- c("males (rho = -0.036, *p* = 0.72)",
               "females (rho = -0.22, *p* = 0.022)")

Berg_Sex <-
  ggplot(data = NachmanTransectsMetadata, aes(x = Absolute_Latitude, y = Body_Weight_g)) +
  geom_smooth(method = "lm", se = FALSE, size = 0.75, aes(linetype = Sex, color = Sex)) +
  geom_jitter(aes(fill = Sex), size = 2, stroke = 0.25, height = 0, width = 0.2, shape = 21, alpha = 0.9, color = "black") +
  scale_color_manual(labels=Berg_deets, values=c("black", "darkgray")) +
  scale_fill_manual(labels=Berg_deets, values=c("black", "white")) +
  scale_linetype_manual(labels=Berg_deets, values = c("solid", "solid")) +
  guides(linetype=guide_legend(override.aes = list(size=1))) +
  scale_x_continuous(breaks = seq(from=0, to=55, by=10), labels = seq(from=0, to=55, by=10), limits = c(0,55)) +
  scale_y_continuous(breaks = seq(from=0, to=30, by=10), labels = seq(from=0, to=30, by=10), limits = c(0,30)) +
  theme_bw() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10), size = 11, face = "bold", family = "Palatino"),
        axis.title.y = element_text(margin = margin(r = 10), size = 11, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 9, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 9, color = "black", family = "Palatino"),
        legend.title = element_blank(),
        legend.position = c(0.22, 0.95),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key.size = unit(0.25, "cm"),
        legend.text = element_markdown(size=7, family = "Palatino"),
        legend.key = element_rect(fill = "transparent"),
        #plot.tag = element_markdown(family = "Palatino", size = 9, color = "black", hjust = 0), # need to use element_markdown since using ggtext
        #plot.tag.position = c(0.2,0.9),
        plot.margin = unit(c(0.5, 0.5, 0, 0.5), "cm")) + #top, right, bottom, left
  #guides(color = guide_legend(nrow = 1)) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5, family = "Palatino")) +
  labs(x = "",
       y = "Body Mass (g)")
#tag = "(R = 0.031, *p* = 0.18)<br /><br />(R = X, *p* = x)",#dobule line break
#subtitle = "A")



Tail_Sex <-
  ggplot(data = NachmanTransects_filtered, aes(x = Absolute_Latitude, y = Resids_TLBW)) +
  geom_smooth(method = "lm", se = FALSE, size = 0.75, aes(linetype = Sex, color = Sex)) +
  geom_jitter(aes(fill = Sex), size = 2, stroke = 0.25, height = 0, width = 0.2, shape = 21, alpha = 0.9, color = "black") +
  scale_color_manual(labels=Tail_deets, values=c("black", "darkgray")) +
  scale_fill_manual(labels=Tail_deets, values=c("black", "white")) +
  scale_linetype_manual(labels=Tail_deets, values = c("solid", "solid")) +
  guides(linetype=guide_legend(override.aes = list(size=1))) +
  scale_x_continuous(breaks = seq(from=0, to=55, by=10), labels = seq(from=0, to=55, by=10), limits = c(0,55)) +
  scale_y_continuous(breaks = seq(from=-30, to=20, by=10), labels = seq(from=-30, to=20, by=10), limits = c(-30,20)) +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10), size = 11, face = "bold", family = "Palatino"),
        axis.title.y = element_text(margin = margin(r = 10), size = 11, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 9, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 9, color = "black", family = "Palatino"),
        legend.title = element_blank(),
        legend.position = c(0.24, 0.95),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key.size = unit(0.25, "cm"),
        legend.text = element_markdown(size=7, family = "Palatino"),
        legend.key = element_rect(fill = "transparent"),
        plot.margin = unit(c(0, 0.5, 0, 0.5), "cm")) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5, family = "Palatino")) +
  labs(x = "",
       y = "Tail Length (mm)")



Ear_Sex <-
  ggplot(data = NachmanTransectsMetadata, aes(x = Absolute_Latitude, y = Resids_ELBW)) +
  geom_smooth(method = "lm", se = FALSE, size = 0.75, aes(linetype = Sex, color = Sex)) +
  geom_jitter(aes(fill = Sex), size = 2, stroke = 0.25, height = 0, width = 0.2, shape = 21, alpha = 0.9, color = "black") +
  scale_color_manual(labels=Ear_deets, values=c("black", "darkgray")) +
  scale_fill_manual(labels=Ear_deets, values=c("black", "white")) +
  scale_linetype_manual(labels=Ear_deets, values = c("solid", "solid")) +
  guides(linetype=guide_legend(override.aes = list(size=1))) +
  scale_x_continuous(breaks = seq(from=0, to=55, by=10), labels = seq(from=0, to=55, by=10), limits = c(0,55)) +
  scale_y_continuous(breaks = seq(from=-5, to=7, by=5), labels = seq(from=-5, to=7, by=5), limits = c(-5,7)) +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10), size = 11, face = "bold", family = "Palatino"),
        axis.title.y = element_text(margin = margin(r = 10), size = 11, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 9, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 9, color = "black", family = "Palatino"),
        legend.title = element_blank(),
        legend.position = c(0.24, 0.95),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key.size = unit(0.25, "cm"),
        legend.text = element_markdown(size=7, family = "Palatino"),
        legend.key = element_rect(fill = "transparent"),
        #plot.tag = element_markdown(family = "Palatino", size = 9, color = "black", hjust = 0), # need to use element_markdown since using ggtext
        #plot.tag.position = c(0.2,0.9),
        plot.margin = unit(c(0, 0.5, 0.5, 0.5), "cm")) +
  #guides(color = guide_legend(nrow = 1)) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5, family = "Palatino")) +
  labs(x = "Degrees from the Equator",
       y = "Ear Length (mm)")


cowplot::plot_grid(Berg_Sex, Tail_Sex, Ear_Sex, labels = c('B', 'D', 'F'), ncol = 1, nrow = 3, label_fontfamily = "Palatino")

ggsave("results/figures/Nachman_transects.tiff", height = 9, width = 4.5, compression = "lzw")
ggsave("results/figures/Nachman_transects.pdf", height = 9, width = 4.5)

dev.off()