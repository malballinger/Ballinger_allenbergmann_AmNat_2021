#!/usr/bin/env Rscript --vanilla

##############################################################
# Author: Mallory A. Ballinger
# Script first created: 26-Feb-2021
# Script last updated:  29-Mar-2021


# This script plots body weight and extremity length from all dowloaded entries
# of North and South American house mice from VertNet.
# This script generates half of Figure 1 in Ballinger_et_al_2021_AmNat


##############################################################
# Required packages
##############################################################

rm(list = ls()) # clear R's environment
library(tidyverse)
library(here)
library(cowplot)
library(ggtext)
library(glue)

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

# get sample size of each column
colSums(!is.na(VertNetMetadata))





##############################################################
# Apply filtering and calculate residuals
##############################################################

# filtering and model testing were completed in 'code/model_VertNetMetadata.R'.
# We are applying the same filtering, models, and statistical outputs here for
# plotting purposes. Please see 'modeling' script for details.


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





##############################################################
# Plot body weight and exremity length vs. abs lat
##############################################################

Berg_deets <- c("males (rho = 0.055, *p* = 0.075)",
                "females (rho = 0.0084, *p* = 0.80)")

Tail_deets <- c("males (rho = -0.23, *p* < 0.0001)",
                "females (rho = -0.23, *p* < 0.0001)")

Ear_deets <- c("males (rho = -0.13, *p* < 0.001)",
                "females (rho = -0.068, *p* = 0.076)")

Berg_Sex <-
  ggplot(data = VertNetMetadata, aes(x = Absolute_Latitude, y = Body_Weight_g)) +
  geom_smooth(method = "lm", se = FALSE, size = 1.3, aes(linetype = Sex, color = Sex)) +
  geom_jitter(aes(fill = Sex), size = 2, stroke = 0.25, height = 0, width = 0.2, shape = 21, alpha = 0.9, color = "black") +
  scale_color_manual(labels=Berg_deets, values=c("black", "darkgray")) +
  scale_fill_manual(labels=Berg_deets, values=c("black", "white")) +
  scale_linetype_manual(labels=Berg_deets, values = c("solid", "solid")) +
  guides(linetype=guide_legend(override.aes = list(size=1))) +
  scale_x_continuous(breaks = seq(from=0, to=65, by=10), labels = seq(from=0, to=65, by=10), limits = c(0,65)) +
  scale_y_continuous(breaks = seq(from=0, to=40, by=10), labels = seq(from=0, to=40, by=10), limits = c(0,40)) +
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
  ggplot(data = VertNet_filtered, aes(x = Absolute_Latitude, y = Resids_TLBW)) +
  geom_smooth(method = "lm", se = FALSE, size = 1.3, aes(linetype = Sex, color = Sex)) +
  geom_jitter(aes(fill = Sex), size = 2, stroke = 0.25, height = 0, width = 0.2, shape = 21, alpha = 0.9, color = "black") +
  scale_color_manual(labels=Tail_deets, values=c("black", "darkgray")) +
  scale_fill_manual(labels=Tail_deets, values=c("black", "white")) +
  scale_linetype_manual(labels=Tail_deets, values = c("solid", "solid")) +
  guides(linetype=guide_legend(override.aes = list(size=1))) +
  scale_x_continuous(breaks = seq(from=0, to=65, by=10), labels = seq(from=0, to=65, by=10), limits = c(0,65)) +
  scale_y_continuous(breaks = seq(from=-50, to=30, by=10), labels = seq(from=-50, to=30, by=10), limits = c(-50,30)) +
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
  ggplot(data = VertNet_filtered_2, aes(x = Absolute_Latitude, y = Resids_ELBW)) +
  geom_smooth(method = "lm", se = FALSE, size = 1.3, aes(linetype = Sex, color = Sex)) +
  geom_jitter(aes(fill = Sex), size = 2, stroke = 0.25, height = 0, width = 0.2, shape = 21, alpha = 0.9, color = "black") +
  scale_color_manual(labels=Ear_deets, values=c("black", "darkgray")) +
  scale_fill_manual(labels=Ear_deets, values=c("black", "white")) +
  scale_linetype_manual(labels=Ear_deets, values = c("solid", "solid")) +
  guides(linetype=guide_legend(override.aes = list(size=1))) +
  scale_x_continuous(breaks = seq(from=0, to=65, by=10), labels = seq(from=0, to=65, by=10), limits = c(0,65)) +
  scale_y_continuous(breaks = seq(from=-10, to=10, by=5), labels = seq(from=-10, to=10, by=5), limits = c(-10,10)) +
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


cowplot::plot_grid(Berg_Sex, Tail_Sex, Ear_Sex, labels = c('A', 'B', 'C'), ncol = 1, nrow = 3, label_fontfamily = "Palatino")

ggsave("results/figures/VertNet_metadata.tiff", height = 9, width = 4.5, compression = "lzw")
ggsave("results/figures/VertNet_metadata.pdf", height = 9, width = 4.5)

dev.off()