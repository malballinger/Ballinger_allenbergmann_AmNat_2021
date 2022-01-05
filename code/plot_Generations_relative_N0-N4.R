#!/usr/bin/env Rscript --vanilla

##############################################################

# Author: Mallory A. Ballinger

# This script plots body mass and relative extremity lengths of New York mice
# and Brazil mice across N0-N4 generations (i.e. common garden experiment #1).
# Data were cleaned using ./clean_Generations_N0-N4.R
# This script generates Figure S2 in Ballinger_AmNat_2021.


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

source("code/model_Generations_relative_N0-N4.R") # use variables created here for plots


##############################################################
# Get values from statistical analyses
##############################################################

# Refer to ./model_Generations_relative_N0-N4.R for model comparisons and statistical analyses

BW_deets <- ("&#42;pop x gen<br>\
             &#42;sex x gen<br>\
             &#42;sex<br>\
             &#42;pop<br>\
             &#42;gen")


TL_deets <- ("&#42;pop x gen<br>\
             &#42;sex x gen<br>\
             &#42;sex<br>\
             &#42;pop<br>\
             &#42;gen")


EL_deets <- ("&#42;pop x gen<br>\
             &#42;sex x gen<br>\
             &#42;sex x pop<br>\
             &#42;sex<br>\
             &#42;pop<br>\
             &#42;gen")


##############################################################
# Plot sex-specific body mass ridgeplot across generations
##############################################################

MaleBW <- GenerationMetaData %>%
  filter(Sex == "Male")

FemaleBW <- GenerationMetaData %>%
  filter(Sex == "Female")


Fmassridge <- 
  ggplot(data = FemaleBW, aes(y = Generation, x = Body_Weight_g)) +
  geom_density_ridges(aes(point_color = Population, point_fill = Population, fill = Population),
                      jittered_points = TRUE, position = position_raincloud(ygap = 0.15)  ,
                      alpha = 0.8, point_size = 1, point_alpha = 0.5, scale = 0.3,
                      point_shape=21, point_color = "white", color = NA) +
  geom_boxplot(aes(x = Body_Weight_g, y = Generation, color = Population),
               position = position_nudge(y = -0.08), outlier.shape = NA,
               alpha = 0.5, width = 0.08, fill = "white") +
  scale_x_continuous(breaks = seq(from=5, to=45, by=10),
                     labels = seq(from=5, to=45, by=10),
                     limits = c(5,45)) +
  scale_discrete_manual("point_fill", values = c("dodgerblue4","goldenrod1"), guide="none") +
  scale_discrete_manual("point_color", values = c("dodgerblue4","goldenrod1"), guide = "none") +
  scale_color_manual(values=c("dodgerblue4","goldenrod1")) +
  scale_fill_manual(values=c("dodgerblue4","goldenrod1", "dodgerblue4","goldenrod1")) +
  guides(color=guide_legend(override.aes=list(fill=NA)),
         linetype=guide_legend(override.aes = list(fill=NA))) +# removes gray shading from legend
  coord_flip() +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.y = element_text(margin = margin(r = 5), size = 9, family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.position = c(0.025, 0.94),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(0.35, "cm"),
        legend.text = element_text(size=7, family = "Palatino"),
        legend.title = element_blank(),
        plot.tag = element_text(family = "Palatino", size = 10, face = "italic", hjust = 1),
        plot.tag.position = c(0.15,1.025),
        plot.title = element_text(size = 8, face = "italic", hjust = 0.05, vjust = -3, family = "Palatino"),
        plot.title.position = "panel",
        plot.margin = unit(c(0, 0, 0, 0.45), "cm")) +
  labs(x = "Body Mass (g)",
       y = NULL,
       title = "Females")


Mmassridge <- 
  ggplot(data = MaleBW, aes(y = Generation, x = Body_Weight_g)) +
  geom_density_ridges(aes(point_color = Population, point_fill = Population, fill = Population),
                      jittered_points = TRUE, position = position_raincloud(ygap = 0.15)  ,
                      alpha = 0.8, point_size = 1, point_alpha = 0.5, scale = 0.3,
                      point_shape=21, point_color = "white", color = NA) +
  geom_boxplot(aes(x = Body_Weight_g, y = Generation, color = Population),
               position = position_nudge(y = -0.08), outlier.shape = NA,
               alpha = 0.5, width = 0.08, fill = "white") +
  scale_x_continuous(breaks = seq(from=5, to=45, by=10),
                     labels = seq(from=5, to=45, by=10),
                     limits = c(5,45)) +
  scale_discrete_manual("point_fill", values = c("dodgerblue4","goldenrod1"), guide="none") +
  scale_discrete_manual("point_color", values = c("dodgerblue4","goldenrod1"), guide = "none") +
  scale_color_manual(values=c("dodgerblue4","goldenrod1")) +
  scale_fill_manual(values=c("dodgerblue4","goldenrod1", "dodgerblue4","goldenrod1")) +
  guides(color=guide_legend(override.aes=list(fill=NA)),
         linetype=guide_legend(override.aes = list(fill=NA))) +
  coord_flip() +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = 'none',
        plot.tag = element_markdown(family = "Palatino", size = 6, face = "italic", hjust = 1),
        plot.tag.position = c(0.98,0.82),
        plot.title = element_text(size = 8, face = "italic", hjust = 0.05, vjust = -3, family = "Palatino"),
        plot.title.position = "panel",
        plot.margin = unit(c(0, 0.75, 0.5, 0), "cm")) + # top, right, bottom, lef
  labs(x = NULL,
       y = NULL,
       title = "Males",
       tag = BW_deets)


BW <- cowplot::plot_grid(Fmassridge, Mmassridge, labels = c('A)', ''), align = "h",
                         label_fontfamily = "Palatino", label_size = 12, label_x = 0.05, hjust = 0)


##############################################################
# Plot sex-specific tail length ridgeplot across generaitons
##############################################################

MaleTL <- Generation_filtered %>%
  filter(Sex == "Male")

FemaleTL <- Generation_filtered %>%
  filter(Sex == "Female")


Ftailridge <- 
  ggplot(data = FemaleTL, aes(y = Generation, x = RelativeTail)) +
  geom_density_ridges(aes(point_color = Population, point_fill = Population, fill = Population),
                      jittered_points = TRUE, position = position_raincloud(ygap = 0.15)  ,
                      alpha = 0.8, point_size = 1, point_alpha = 0.5, scale = 0.3,
                      point_shape=21, point_color = "white", color = NA) +
  geom_boxplot(aes(x = RelativeTail, y = Generation, color = Population),
               position = position_nudge(y = -0.08), outlier.shape = NA,
               alpha = 0.5, width = 0.08, fill = "white") +
  scale_x_continuous(breaks = seq(from=1.5, to=9, by=3),
                     labels = seq(from=1.5, to=9, by=3),
                     limits = c(1.5,9)) +
  scale_discrete_manual("point_fill", values = c("dodgerblue4","goldenrod1"), guide="none") +
  scale_discrete_manual("point_color", values = c("dodgerblue4","goldenrod1"), guide = "none") +
  scale_color_manual(values=c("dodgerblue4","goldenrod1")) +
  scale_fill_manual(values=c("dodgerblue4","goldenrod1", "dodgerblue4","goldenrod1")) +
  guides(color=guide_legend(override.aes=list(fill=NA)),
         linetype=guide_legend(override.aes = list(fill=NA))) +# removes gray shading from legend
  coord_flip() +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 10), size = 10, family = "Palatino", hjust = 1.15),
        axis.title.y = element_text(margin = margin(r = 3), size = 8, family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.position = "none",
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.5, "cm"),
        legend.text = element_text(size=8, family = "Palatino"),
        legend.title = element_blank(),
        plot.tag = element_text(family = "Palatino", size = 10, face = "italic"),
        plot.tag.position = c(0.17,1.025),
        plot.title = element_text(size = 8, face = "italic", hjust = 0.05, vjust = -3, family = "Palatino"),
        plot.title.position = "panel",
        plot.margin = unit(c(0, 0, 0, 0.375), "cm")) +
  labs(x = "Relative Tail Length (mm/g)",
       y = NULL,
       title = "Females")


Mtailridge <- 
  ggplot(data = MaleTL, aes(y = Generation, x = RelativeTail)) +
  geom_density_ridges(aes(point_color = Population, point_fill = Population, fill = Population),
                      jittered_points = TRUE, position = position_raincloud(ygap = 0.15)  ,
                      alpha = 0.8, point_size = 1, point_alpha = 0.5, scale = 0.3,
                      point_shape=21, point_color = "white", color = NA) +
  geom_boxplot(aes(x = RelativeTail, y = Generation, color = Population),
               position = position_nudge(y = -0.08), outlier.shape = NA,
               alpha = 0.5, width = 0.08, fill = "white") +
  scale_x_continuous(breaks = seq(from=1.5, to=9, by=3),
                     labels = seq(from=1.5, to=9, by=3),
                     limits = c(1.5,9)) +
  scale_discrete_manual("point_fill", values = c("dodgerblue4","goldenrod1"), guide="none") +
  scale_discrete_manual("point_color", values = c("dodgerblue4","goldenrod1"), guide = "none") +
  scale_color_manual(values=c("dodgerblue4","goldenrod1")) +
  scale_fill_manual(values=c("dodgerblue4","goldenrod1", "dodgerblue4","goldenrod1")) +
  guides(color=guide_legend(override.aes=list(fill=NA)),
         linetype=guide_legend(override.aes = list(fill=NA))) +
  coord_flip() +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 10), size = 10, family = "Palatino", hjust = 0.6),
        axis.title.y = element_text(margin = margin(r = 10), size = 10, family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = 'none',
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.5, "cm"),
        legend.text = element_text(size=8, family = "Palatino"),
        legend.title = element_blank(),
        plot.tag = element_markdown(family = "Palatino", size = 6, face = "italic", hjust = 1),
        plot.tag.position = c(0.98,0.81),
        plot.title = element_text(size = 8, face = "italic", hjust = 0.05, vjust = -3, family = "Palatino"),
        plot.title.position = "panel",
        plot.margin = unit(c(0, 0.75, 0.5, 0), "cm")) + # top, right, bottom, left (0.5, 0.5, 0.1, -0.5)
  labs(x = NULL,
       y = NULL,
       title = "Males",
       tag = TL_deets)


Tails <- cowplot::plot_grid(Ftailridge, Mtailridge, labels = c('B)', ''), align = "h",
                            label_fontfamily = "Palatino", label_size = 12, label_x = 0.05, hjust = 0)


##############################################################
# Plot sex-specific ear length ridgeplot across generaitons
##############################################################

MaleEL <- Generation_filtered %>%
  filter(Sex == "Male")

FemaleEL <- Generation_filtered %>%
  filter(Sex == "Female")


Fearridge <- 
  ggplot(data = FemaleEL, aes(y = Generation, x = RelativeEar)) +
  geom_density_ridges(aes(point_color = Population, point_fill = Population, fill = Population),
                      jittered_points = TRUE, position = position_raincloud(ygap = 0.15)  ,
                      alpha = 0.8, point_size = 1, point_alpha = 0.5, scale = 0.3,
                      point_shape=21, point_color = "white", color = NA) +
  geom_boxplot(aes(x = RelativeEar, y = Generation, color = Population),
               position = position_nudge(y = -0.08), outlier.shape = NA,
               alpha = 0.5, width = 0.08, fill = "white") +
  scale_x_continuous(breaks = seq(from=0.2, to=1.6, by=0.6),
                     labels = seq(from=0.2, to=1.6, by=0.6),
                     limits = c(0.2,1.6)) +
  scale_discrete_manual("point_fill", values = c("dodgerblue4","goldenrod1"), guide="none") +
  scale_discrete_manual("point_color", values = c("dodgerblue4","goldenrod1"), guide = "none") +
  scale_color_manual(values=c("dodgerblue4","goldenrod1")) +
  scale_fill_manual(values=c("dodgerblue4","goldenrod1", "dodgerblue4","goldenrod1")) +
  guides(color=guide_legend(override.aes=list(fill=NA)),
         linetype=guide_legend(override.aes = list(fill=NA))) + # removes gray shading from legend
  coord_flip() +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 12), size = 10, family = "Palatino", hjust = 1.2),
        axis.title.y = element_text(margin = margin(r = 3), size = 8, family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.position = "none",
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.5, "cm"),
        legend.text = element_text(size=8, family = "Palatino"),
        legend.title = element_blank(),
        plot.tag = element_text(family = "Palatino", size = 10, face = "italic"),
        plot.tag.position = c(0.17,1.025),
        plot.title = element_text(size = 8, face = "italic", hjust = 0.05, vjust = -3, family = "Palatino"),
        plot.title.position = "panel",
        plot.margin = unit(c(0, 0, 0, 0.5), "cm")) +
  labs(x = "Relative Ear Length (mm/g)",
       y = "Generation",
       title = "Females")


Mearridge <- 
  ggplot(data = MaleEL, aes(y = Generation, x = RelativeEar)) +
  geom_density_ridges(aes(point_color = Population, point_fill = Population, fill = Population),
                      jittered_points = TRUE, position = position_raincloud(ygap = 0.15)  ,
                      alpha = 0.8, point_size = 1, point_alpha = 0.5, scale = 0.3,
                      point_shape=21, point_color = "white", color = NA) +
  geom_boxplot(aes(x = RelativeEar, y = Generation, color = Population),
               position = position_nudge(y = -0.08), outlier.shape = NA,
               alpha = 0.5, width = 0.08, fill = "white") +
  scale_x_continuous(breaks = seq(from=0.2, to=1.6, by=0.6),
                     labels = seq(from=0.2, to=1.6, by=0.6),
                     limits = c(0.2,1.6)) +
  scale_discrete_manual("point_fill", values = c("dodgerblue4","goldenrod1"), guide="none") +
  scale_discrete_manual("point_color", values = c("dodgerblue4","goldenrod1"), guide = "none") +
  scale_color_manual(values=c("dodgerblue4","goldenrod1")) +
  scale_fill_manual(values=c("dodgerblue4","goldenrod1", "dodgerblue4","goldenrod1")) +
  guides(color=guide_legend(override.aes=list(fill=NA)),
         linetype=guide_legend(override.aes = list(fill=NA))) +
  coord_flip() +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 10), size = 10, family = "Palatino", hjust = 0.6),
        axis.title.y = element_text(margin = margin(r = 10), size = 9, family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = 'none',
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.5, "cm"),
        legend.text = element_text(size=8, family = "Palatino"),
        legend.title = element_blank(),
        plot.tag = element_markdown(family = "Palatino", size = 6, face = "italic", hjust = 1),
        plot.tag.position = c(0.98,0.82),
        plot.title = element_text(size = 8, face = "italic", hjust = 0.05, vjust = -3, family = "Palatino"),
        plot.title.position = "panel",
        plot.margin = unit(c(-0.1, 0.75, -0.25, 0), "cm")) + # top, right, bottom, left (0.5, 0.5, 0.1, -0.5)
  labs(x = NULL,
       y = NULL,
       title = "Males",
       tag = EL_deets)


Ears <- cowplot::plot_grid(Fearridge, Mearridge, labels = c('C)', ''), align = 'h',
                           label_fontfamily = "Palatino", label_size = 12, label_x = 0.05, hjust = 0)



cowplot::plot_grid(BW, Tails, Ears, ncol=1, nrow=3, label_fontfamily = "Palatino",
                   label_size = 12, label_x = 0.05, hjust = 0, align = 'v')

#ggsave("Generations_relative_N0-N4.tiff", path = "results/figures", height = 7, width = 6, compression = "lzw")
ggsave("results/figures/Generations_relative_N0-N4.pdf", height = 7, width = 6)
