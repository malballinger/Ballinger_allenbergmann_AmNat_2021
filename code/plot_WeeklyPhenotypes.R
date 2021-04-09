#!/usr/bin/env Rscript --vanilla

##############################################################
# Author: Mallory A. Ballinger
# Script first created: 12-Feb-2021
# Script last updated:  09-Apr-2021


# This script plots weekly body weight and tail lengths of New York mice and 
# Brazil mice across environments (i.e., common garden experiment #2).
# This script generates Figures 3 and 4 in Ballinger_et_al_2021_AmNat.


##############################################################
# Required packages
##############################################################

rm(list = ls()) # clear R's environment
library(tidyverse)
library(here)
library(ggridges)
library(cowplot)
library(car)
library(ggtext)
library(glue)
library(scales)

##############################################################
# Import data
##############################################################

WeeklyPhenotypeData <- read_csv(here("data/processed/WeeklyPhenotypeData.csv")) %>%
  select(-DOB, -DateMeasured) %>% select(-1) %>%
  mutate(Line = fct_relevel(Line, "SARA", "SARB", "MANA", "MANB")) %>% # put evolved lines first
  mutate(Environment = fct_relevel(Environment, "Warm", "Cold" )) %>% # put Warm before Cold
  mutate(PopEnv = paste(Population, Environment, sep = "_")) %>%
  mutate(PopEnv = fct_recode(PopEnv, "Brazil - Warm" = "Brazil_Warm", "Brazil - Cold" = "Brazil_Cold",
                             "New York - Warm" = "New York_Warm", "New York - Cold" = "New York_Cold")) %>%
  mutate(PopEnv = fct_relevel(PopEnv, "New York - Warm", "New York - Cold", "Brazil - Warm", "Brazil - Cold"))


# Sample size of dataset
#FullSampleSize <- WeeklyPhenotypeData %>% summarise(N=n_distinct(Mouse_ID)) %>% pull(N)
# n = 80


# Make sex-specific datasets
MaleData <- WeeklyPhenotypeData %>%
  filter(Sex == "Male")

FemaleData <- WeeklyPhenotypeData %>%
  filter(Sex == "Female")





##############################################################
# Plot weekly phenotypes - body weight
##############################################################

FemaleBW <-
  ggplot(data=FemaleData, aes(x=Age_weeks, y=Body_Weight_g, fill = PopEnv, linetype = PopEnv, color = PopEnv)) +
  geom_point(position = position_dodge(0.1), size=1.1, alpha = 0.5, show.legend = FALSE) +
  geom_smooth(aes(group = PopEnv), size=1.5, alpha = 0.15, method = "loess", se = TRUE) + #std error
  scale_color_manual(values=rep(c("dodgerblue4", "dodgerblue3", "goldenrod3", "goldenrod1"))) +
  scale_fill_manual(values=rep(c("dodgerblue4", "dodgerblue3", "goldenrod3","goldenrod1"))) +
  scale_linetype_manual(values=rep(c("solid","dashed", "solid", "dashed"))) +
  guides(color=guide_legend(override.aes=list(fill=NA)),
         linetype=guide_legend(override.aes = list(size=0.5)),
         linetype=guide_legend(override.aes = list(fill=NA))) +# removes gray shading from legend
  scale_x_continuous(breaks = seq(from=3, to=12, by=2), labels = seq(from=3, to=12, by=2), limits = c(3,11.3)) +
  scale_y_continuous(breaks = seq(from=5, to=25, by=5), labels = seq(from=5, to=25, by=5), limits = c(5,25)) +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 10), size = 10, face = "bold", family = "Palatino", hjust = 1.25),
        axis.title.y = element_text(margin = margin(r = 10), size = 10, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.position = c(0.03, 0.89),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key.height = unit(0.2, "cm"),
        legend.key.width = unit(0.6, "cm"),
        legend.margin = margin(0.1, 3, 0.3, 0),
        legend.spacing.x = unit(0.5, "mm"),
        legend.spacing.y = unit(0.5, "mm"),
        legend.text = element_text(size=6.5, family = "Palatino"),
        legend.title = element_blank(),
        plot.tag = element_text(family = "Palatino", size = 10, face = "italic"),
        plot.tag.position = c(0.18,1.025),
        plot.title = element_text(size = 8, face = "italic", hjust = 0.05, vjust = -3, family = "Palatino"),
        plot.title.position = "panel",
        plot.margin = unit(c(0.5, -0.45, 0, 0.3), "cm")) +
  labs(x = "Age (weeks)",
       y = "Body Mass (g)",
       title = "Females")


MaleBW <-
  ggplot(data=MaleData, aes(x=Age_weeks, y=Body_Weight_g, fill = PopEnv, linetype = PopEnv, color = PopEnv)) +
  geom_point(position = position_dodge(0.1), size=1.1, alpha=0.5, show.legend = FALSE) +
  geom_smooth(aes(group = PopEnv), size=1.5, method = "loess", alpha = 0.15, se = TRUE) + #std error
  scale_color_manual(values=rep(c("dodgerblue4", "dodgerblue3", "goldenrod3", "goldenrod1"))) +
  scale_fill_manual(values=rep(c("dodgerblue4", "dodgerblue3", "goldenrod3","goldenrod1"))) +
  scale_linetype_manual(values=rep(c("solid","dashed", "solid", "dashed"))) +
  scale_x_continuous(breaks = seq(from=3, to=12, by=2), labels = seq(from=3, to=12, by=2), limits = c(3,11.3)) +
  scale_y_continuous(breaks = seq(from=5, to=25, by=5), labels = seq(from=5, to=25, by=5), limits = c(5,25)) +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 10), size = 10, face = "bold", family = "Palatino"),
        axis.title.y = element_text(margin = margin(r = 10), size = 10, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "none",
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(1.5, "cm"),
        legend.text = element_text(size=8, family = "Palatino"),
        legend.title = element_blank(),
        plot.tag = element_text(family = "Palatino", size = 10, face = "italic"),
        plot.tag.position = c(0.13,1.025),
        plot.title = element_text(size = 8, face = "italic", hjust = 0.05, vjust = -3, family = "Palatino"),
        plot.title.position = "panel",
        plot.margin = unit(c(0.5, 0.5, 0, -0.42), "cm")) + # top, right, bottom, left (0.5, 0.5, 0.1, -0.5)
  labs(x = "",
       y = "",
       title = "Males")


cowplot::plot_grid(FemaleBW, MaleBW, labels = c('', ''), align = 'h',
                   label_fontfamily = "Palatino", label_size = 12, label_x = 0.05, hjust = 0)

ggsave("results/figures/Weekly_BW.tiff", height = 3.5, width = 6, compression = "lzw")
ggsave("results/figures/Weekly_BW.pdf", height = 3.5, width = 6)





##############################################################
# Plot weekly phenotypes - tail length
##############################################################

FemaleTL <-
  ggplot(data=FemaleData, aes(x=Age_weeks, y=Tail_Length_mm, color = PopEnv, fill = PopEnv, linetype = PopEnv)) +
  geom_point(position = position_dodge(0.1), size=1.1, alpha=0.5, show.legend = FALSE) +
  geom_smooth(aes(group = PopEnv), size=1.5, method = "loess", alpha = 0.15, se = TRUE) + #std error
  scale_color_manual(values=rep(c("dodgerblue4", "dodgerblue3", "goldenrod3", "goldenrod1"))) +
  scale_fill_manual(values=rep(c("dodgerblue4", "dodgerblue3", "goldenrod3","goldenrod1"))) +
  scale_linetype_manual(values=rep(c("solid","dashed", "solid", "dashed"))) +
  guides(color=guide_legend(override.aes=list(fill=NA)),
         linetype=guide_legend(override.aes = list(size=0.5)),
         linetype=guide_legend(override.aes = list(fill=NA))) +# removes gray shading from legend
  scale_x_continuous(breaks = seq(from=3, to=12, by=2), labels = seq(from=3, to=12, by=2), limits = c(3,11.3)) +
  scale_y_continuous(breaks = seq(from=45, to=90, by=10), labels = seq(from=45, to=90, by=10), limits = c(45,85)) +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 10), size = 10, face = "bold", family = "Palatino"),
        axis.title.y = element_text(margin = margin(r = 10), size = 10, face = "bold", family = "Palatino", hjust = -1),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.position = c(0.6, 0.15),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(0.2, "cm"),
        legend.key.width = unit(0.6, "cm"),
        legend.margin = margin(0.1, 3, 0.3, 0),
        legend.spacing.x = unit(0.5, "mm"),
        legend.spacing.y = unit(0.5, "mm"),
        legend.text = element_text(size=6.5, family = "Palatino"),
        legend.title = element_blank(),
        plot.tag = element_text(family = "Palatino", size = 8, face = "italic"),
        plot.tag.position = c(0.18,1.025),
        plot.title = element_text(size = 8, face = "italic", hjust = 0.05, vjust = -3, family = "Palatino"),
        plot.title.position = "panel",
        plot.margin = unit(c(0, 0.5, -0.3, 0.5), "cm")) +
  labs(x = "",
       y = "Tail Length (mm)",
       title = "Females")
       #tag = "Females")

MaleTL <-
  ggplot(data=MaleData, aes(x=Age_weeks, y=Tail_Length_mm, fill = PopEnv, linetype = PopEnv, color = PopEnv)) +
  geom_point(position = position_dodge(0.1), size=1.1, alpha=0.5, show.legend = FALSE) +
  geom_smooth(aes(group = PopEnv), size=1.5, method = "loess", alpha = 0.15, se = TRUE) + #std error
  scale_color_manual(values=rep(c("dodgerblue4", "dodgerblue3", "goldenrod3", "goldenrod1"))) +
  scale_fill_manual(values=rep(c("dodgerblue4", "dodgerblue3", "goldenrod3","goldenrod1"))) +
  scale_linetype_manual(values=rep(c("solid","dashed", "solid", "dashed"))) +
  scale_x_continuous(breaks = seq(from=3, to=12, by=2), labels = seq(from=3, to=12, by=2), limits = c(3,11.3)) +
  scale_y_continuous(breaks = seq(from=45, to=90, by=10), labels = seq(from=45, to=90, by=10), limits = c(45,85)) +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 10), size = 10, face = "bold", family = "Palatino"),
        axis.title.y = element_text(margin = margin(r = 10), size = 10, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.position = "none",
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(1.5, "cm"),
        legend.text = element_text(size=7, family = "Palatino"),
        legend.title = element_blank(),
        plot.tag = element_text(family = "Palatino", size = 10, face = "italic"),
        plot.tag.position = c(0.13,1.025),
        plot.title = element_text(size = 8, face = "italic", hjust = 0.05, vjust = -3, family = "Palatino"),
        plot.title.position = "panel",
        plot.margin = unit(c(-0.3, 0.5, 0, 0.5), "cm")) + # top, right, bottom, left (0.5, 0.5, 0.1, -0.5)
  labs(x = "Age (weeks)",
       y = "",
       title = "Males")
       #tag ="Males")


cowplot::plot_grid(FemaleTL, MaleTL, labels = c('', ''), nrow = 2, ncol = 1, align = 'v',
                   label_fontfamily = "Palatino", label_size = 12, label_x = 0.05, hjust = 0)

ggsave("results/figures/Weekly_Tails.tiff", height = 5.25, width = 3.75, compression = "lzw")
ggsave("results/figures/Weekly_Tails.pdf", height = 5.25, width = 3.75)
