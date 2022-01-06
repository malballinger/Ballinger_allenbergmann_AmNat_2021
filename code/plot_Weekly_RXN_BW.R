#!/usr/bin/env Rscript --vanilla

##############################################################

# Author: Mallory A. Ballinger

# This script plots weekly body mass of New York mice and 
# Brazil mice across environments (i.e., common garden experiment #2).
# This script also plots reaction norms of body mass from data collected
# at the end of common garden experiment #2.
# This script generates Figure 3 in Ballinger_AmNat_2021.


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

set.seed(19910118) # so that jitter plots stay in same jittered positions


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
  mutate(PopEnv = fct_relevel(PopEnv, "New York - Warm", "New York - Cold", "Brazil - Warm", "Brazil - Cold")) %>%
  mutate(RelativeTail = Tail_Length_mm / Body_Weight_g)


PostDissectionMetaData <- read_csv(here("data/processed/PostDissectionMetaData.csv")) %>%
  select(-1) %>% # removes first column
  mutate(Sex = fct_relevel(Sex, "Male", "Female")) %>% # put males before females
  mutate(Population = fct_relevel(Population, "New York", "Brazil")) %>% # put evolved cold pop first
  mutate(Line = fct_relevel(Line, "SARA", "SARB", "MANA", "MANB")) %>% # put evolved cold lines first
  mutate(Environment = fct_relevel(Environment, "Warm", "Cold" )) %>% # put warm before cold (evolved before plasticity)
  mutate(RelativeTail = ((Final_Tail_Length_mm) / (Body_Weight_g)))


##############################################################
# Plot weekly phenotypes - body weight
##############################################################

# Make sex-specific datasets
MaleData <- WeeklyPhenotypeData %>%
  filter(Sex == "Male")

FemaleData <- WeeklyPhenotypeData %>%
  filter(Sex == "Female")

FemaleBW <-
  ggplot(data=FemaleData, aes(x=Age_weeks, y=Body_Weight_g, fill = PopEnv, linetype = PopEnv, color = PopEnv)) +
  geom_point(aes(shape = factor(PopEnv)), position = position_dodge(0.1), size=0.9, alpha = 0.7, show.legend = TRUE) +
  geom_smooth(aes(group = PopEnv), size=1.5, alpha = 0.15, method = "loess", se = TRUE) + #std error
  scale_color_manual(values=rep(c("dodgerblue4", "dodgerblue3", "goldenrod3", "goldenrod1"))) +
  scale_fill_manual(values=rep(c("dodgerblue4", "dodgerblue3", "goldenrod3","goldenrod1"))) +
  scale_linetype_manual(values=rep(c("solid","dashed", "solid", "dashed"))) +
  scale_shape_manual(values=rep(c(19, 1, 19, 1)), guide = "none") +
  guides(color=guide_legend(override.aes=list(fill=NA)),
         linetype=guide_legend(override.aes = list(size=0.5)),
         linetype=guide_legend(override.aes = list(fill=NA))) + # removes gray shading from legend
  scale_x_continuous(breaks = seq(from=3, to=12, by=2), labels = seq(from=3, to=12, by=2), limits = c(3,11.3)) +
  scale_y_continuous(breaks = seq(from=5, to=25, by=5), labels = seq(from=5, to=25, by=5), limits = c(5,25)) +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 10), size = 10, family = "Palatino", hjust = 1.2),
        axis.title.y = element_text(margin = margin(r = 10), size = 10, family = "Palatino"),
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
  geom_point(aes(shape = factor(PopEnv)), position = position_dodge(0.1), alpha=0.7, size=0.9, show.legend = TRUE) +
  geom_smooth(aes(group = PopEnv), size=1.5, method = "loess", alpha = 0.15, se = TRUE) + #std error
  scale_color_manual(values=rep(c("dodgerblue4", "dodgerblue3", "goldenrod3", "goldenrod1"))) +
  scale_fill_manual(values=rep(c("dodgerblue4", "dodgerblue3", "goldenrod3","goldenrod1"))) +
  scale_linetype_manual(values=rep(c("solid","dashed", "solid", "dashed"))) +
  scale_shape_manual(values=rep(c(19, 1, 19, 1))) +
  scale_x_continuous(breaks = seq(from=3, to=12, by=2), labels = seq(from=3, to=12, by=2), limits = c(3,11.3)) +
  scale_y_continuous(breaks = seq(from=5, to=25, by=5), labels = seq(from=5, to=25, by=5), limits = c(5,25)) +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 10), size = 10, family = "Palatino"),
        axis.title.y = element_text(margin = margin(r = 10), size = 10, family = "Palatino"),
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


weekly_BW <- cowplot::plot_grid(FemaleBW, MaleBW, labels = c('', ''), align = 'h',
                   label_fontfamily = "Palatino", label_size = 12, label_x = 0.05, hjust = 0)


##############################################################
# Plot body weight RXNs
##############################################################

BW_deets <- ("&#42;sex<br>\
              &#42;pop")


# Sex-specific datasets
MaleData_RXN <- PostDissectionMetaData %>%
  filter(Sex == "Male")

FemaleData_RXN <- PostDissectionMetaData %>%
  filter(Sex == "Female")


BWrxnF <-
  ggplot(data=FemaleData_RXN, aes(x=Environment, y=Body_Weight_g)) +
  geom_boxplot(aes(fill = Population), position = position_dodge(width=0.25), alpha=0.8, size = 0.5, outlier.shape = NA, notch = FALSE, coef=0) +
  stat_summary(
    fun = median,
    geom = 'line',
    aes(group = Population, color = Population),
    position = position_dodge(width = 0.25) , size=1.5, alpha = 1.2, linetype = "dashed") +
  geom_point(aes(fill = Population), position = position_jitterdodge(jitter.width = 0.15, dodge.width=0.25), alpha=5, size=2, shape=21, color = "white") +
  scale_color_manual(values=rep(c("dodgerblue4", "goldenrod1"))) +
  scale_fill_manual(values=rep(c("dodgerblue4", "goldenrod1"))) +
  scale_y_continuous(breaks = seq(from=8, to=24, by=4), labels = seq(from=8, to=24, by=4), limits = c(8,25)) +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10), size = 11, family = "Palatino"),
        axis.text.x = element_text(size = 9, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.position = c(0.047, 0.93),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.margin = margin(0,0,0,0),
        legend.spacing.x = unit(0.1, "cm"),
        legend.key.height = unit(0.35, "cm"),
        legend.key.width = unit(0.3, "cm"),
        legend.text = element_text(size=7, family = "Palatino"),
        legend.title = element_blank(),
        plot.tag = element_text(family = "Palatino", size = 8, face = "italic"),
        plot.tag.position = c(0.2,1.015),
        plot.title = element_text(size = 8, face = "italic", hjust = 0.05, vjust = -3, family = "Palatino"),
        plot.title.position = "panel",
        plot.margin = unit(c(0.5, -0.45, 0.5, 0.3), "cm")) +
  labs(x = "",
       y = "Body Mass (g)",
       title = "Females")


BWrxnM <-
  ggplot(data=MaleData_RXN, aes(x=Environment, y=Body_Weight_g)) +
  geom_boxplot(aes(fill = Population), position = position_dodge(width=0.25), alpha=0.8, size = 0.5, outlier.shape = NA, notch = FALSE, coef=0) +
  stat_summary(
    fun = median,
    geom = 'line',
    aes(group = Population, color = Population),
    position = position_dodge(width = 0.25) , size=1.5, alpha = 1.2, linetype = "dashed") +
  geom_point(aes(fill = Population), position = position_jitterdodge(jitter.width = 0.15, dodge.width=0.25), alpha=5, size=2, shape=21, color = "white") +
  scale_color_manual(values=rep(c("dodgerblue4", "goldenrod1"))) +
  scale_fill_manual(values=rep(c("dodgerblue4", "goldenrod1"))) +
  scale_y_continuous(breaks = seq(from=8, to=24, by=4), labels = seq(from=8, to=24, by=4), limits = c(8,25)) +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10), size = 10, family = "Palatino"),
        axis.text.x = element_text(size = 9, color = "black", family = "Palatino"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "none",
        plot.tag = element_markdown(family = "Palatino", size = 8, face = "italic", hjust = 1),
        plot.tag.position = c(0.96,0.86),
        plot.title = element_text(size = 8, face = "italic", hjust = 0.05, vjust = -3, family = "Palatino"),
        plot.title.position = "panel",
        plot.margin = unit(c(0.5, 0.5, 0.5, -0.42), "cm")) + # top, right, bottom, left (0.5, 0.5, 0.1, -0.5)
  labs(x = "",
       y = "",
       title = "Males",
       tag = BW_deets)


rxn_BW <- cowplot::plot_grid(BWrxnF, BWrxnM, align = "h", label_fontfamily = "Palatino",
                         label_size = 12, label_x = 0.05, hjust = 0)

cowplot::plot_grid(weekly_BW, rxn_BW, nrow = 2, ncol = 1, align = "hv", label_fontfamily = "Palatino",
                                    labels = c('A)', 'B)'), label_size = 12, label_x = 0, label_y = 0.88, hjust = 0, vjust = 0)


#ggsave("results/figures/Weekly_RXN_BW.tiff", height = 6, width = 6, compression = "lzw")
ggsave("results/figures/Weekly_RXN_BW.pdf", height = 5.5, width = 5.5)

