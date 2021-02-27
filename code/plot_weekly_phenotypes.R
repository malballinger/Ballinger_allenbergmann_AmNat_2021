#!/usr/bin/env Rscript --vanilla

##############################################################
# Author: Mallory A. Ballinger
# Script first created: 12-Feb-2021
# Script last updated:  25-Feb-2021


# This script plots weekly body weight and tail lengths of New York and Brazil mice across environments,
# and generates Fig. XXXX in Ballinger_et_al_2021_AmNat


##############################################################
# Required packages
##############################################################

rm(list = ls()) # clears R's environment
library(tidyverse)
library(here)
library(cowplot)
library(car)

##############################################################
# Import data
##############################################################

WeeklyMetaData <- read_csv(here("data/raw/weekly_metadata_RAW_2021-02-11.csv")) %>%
  filter(Population == "BRAZIL" | Population == "NEW_YORK") %>% # only keep parental populations (remove F1 hybrids)
  filter(Generation == "N11" | Generation == "N12") %>%
  mutate(Sex = fct_recode(Sex, "Female" = "F", "Male" = "M")) %>% # spells out males and females
  mutate(Environment = fct_recode(Environment, "Cold" = "COLD", "Warm" = "RT")) %>%
  mutate(Line = fct_recode(Line, "MANA" = "193x255", "SARA" = "19x13",
                           "MANB" = "222x254", "SARB" = "82x81")) %>% # gives each line the "published"/JAX name
  mutate(Population = fct_recode(Population, "Brazil" = "BRAZIL", "NewYork" = "NEW_YORK")) %>% # modifies names of populations
  mutate(Line = fct_relevel(Line, "MANA", "MANB", "SARA", "SARB")) %>% # puts Brazil lines before New York lines
  mutate(Environment = fct_relevel(Environment, "Warm", "Cold" )) %>% # puts Warm before Cold
  mutate(PopEnv = paste(Population, Environment, sep = "_")) %>%
  mutate(PopEnv = fct_recode(PopEnv, "Brazil - Warm" = "Brazil_Warm", "Brazil - Cold" = "Brazil_Cold",
                             "New York - Warm" = "NewYork_Warm", "New York - Cold" = "NewYork_Cold")) %>%
  mutate(PopEnv = fct_relevel(PopEnv, "New York - Warm", "New York - Cold", "Brazil - Warm", "Brazil - Cold"))


# Make sex-specific datasets
MaleData <- WeeklyMetaData %>%
  filter(Sex == "Male")

FemaleData <- WeeklyMetaData %>%
  filter(Sex == "Female")


##############################################################
# Plot weekly phenotypes (body weight & tail length)
##############################################################


MaleBW <-
  ggplot(data=MaleData, aes(x=Age_weeks, y=BodyWeight_g, fill = PopEnv, linetype = PopEnv, color = PopEnv)) +
  geom_point(position = position_dodge(0.1), size=1.1, alpha = 0.5, show.legend = FALSE) +
  geom_smooth(aes(group = PopEnv), size=1.7, alpha = 0.15, method = "loess", se = TRUE) + #std error
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
        axis.title.x = element_text(margin = margin(t = 10), size = 11, face = "bold", family = "Palatino", hjust = 1.25),
        axis.title.y = element_text(margin = margin(r = 10), size = 11, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 10, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 10, color = "black", family = "Palatino"),
        legend.position = c(0.03, 0.91),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(1.25, "cm"),
        legend.text = element_text(size=7, family = "Palatino"),
        legend.title = element_blank(),
        plot.tag = element_text(family = "Palatino", size = 10, face = "italic"),
        plot.tag.position = c(0.18,1.025),
        plot.title = element_text(family = "Palatino", face = "plain"),
        plot.margin = unit(c(0.5, -0.5, 0.1, 0.5), "cm")) +
  labs(x = "",
       y = "Body Mass (g)",
       tag = "Males")

FemaleBW <-
  ggplot(data=FemaleData, aes(x=Age_weeks, y=BodyWeight_g, fill = PopEnv, linetype = PopEnv, color = PopEnv)) +
  geom_point(position = position_dodge(0.1), size=1.1, alpha=0.5, show.legend = FALSE) +
  geom_smooth(aes(group = PopEnv), size=1.7, method = "loess", alpha = 0.15, se = TRUE) + #std error
  scale_color_manual(values=rep(c("dodgerblue4", "dodgerblue3", "goldenrod3", "goldenrod1"))) +
  scale_fill_manual(values=rep(c("dodgerblue4", "dodgerblue3", "goldenrod3","goldenrod1"))) +
  scale_linetype_manual(values=rep(c("solid","dashed", "solid", "dashed"))) +
  scale_x_continuous(breaks = seq(from=3, to=12, by=2), labels = seq(from=3, to=12, by=2), limits = c(3,11.3)) +
  scale_y_continuous(breaks = seq(from=5, to=25, by=5), labels = seq(from=5, to=25, by=5), limits = c(5,25)) +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 10), size = 11, face = "bold", family = "Palatino"),
        axis.title.y = element_text(margin = margin(r = 10), size = 11, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 10, color = "black", family = "Palatino"),
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
        plot.title = element_text(family = "Palatino", face = "plain"),
        plot.margin = unit(c(0.5, 0.5, 0.1, -0.5), "cm")) + # top, right, bottom, left (0.5, 0.5, 0.1, -0.5)
  labs(x = "",
       y = "",
       tag ="Females")

MaleTL <-
  ggplot(data=MaleData, aes(x=Age_weeks, y=TailLength_mm, color = PopEnv, fill = PopEnv, linetype = PopEnv)) +
  geom_point(position = position_dodge(0.1), size=1.1, alpha=0.5, show.legend = FALSE) +
  geom_smooth(aes(group = PopEnv), size=1.7, method = "loess", alpha = 0.15, se = TRUE) + #std error
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
        axis.title.x = element_text(margin = margin(t = 10), size = 11, face = "bold", family = "Palatino", hjust = 1.25),
        axis.title.y = element_text(margin = margin(r = 10), size = 11, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 10, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 10, color = "black", family = "Palatino"),
        legend.position = "none",
        plot.tag = element_text(family = "Palatino", size = 10, face = "italic"),
        plot.tag.position = c(0.18,1.025),
        plot.title = element_text(family = "Palatino", face = "plain"),
        plot.margin = unit(c(0.1, -0.5, 0.1, 0.5), "cm")) +
  labs(x = "Age (weeks)",
       y = "Tail Length (mm)",
       tag = "Males")

FemaleTL <-
  ggplot(data=FemaleData, aes(x=Age_weeks, y=TailLength_mm, fill = PopEnv, linetype = PopEnv, color = PopEnv)) +
  geom_point(position = position_dodge(0.1), size=1.1, alpha=0.5, show.legend = FALSE) +
  geom_smooth(aes(group = PopEnv), size=1.7, method = "loess", alpha = 0.15, se = TRUE) + #std error
  scale_color_manual(values=rep(c("dodgerblue4", "dodgerblue3", "goldenrod3", "goldenrod1"))) +
  scale_fill_manual(values=rep(c("dodgerblue4", "dodgerblue3", "goldenrod3","goldenrod1"))) +
  scale_linetype_manual(values=rep(c("solid","dashed", "solid", "dashed"))) +
  scale_x_continuous(breaks = seq(from=3, to=12, by=2), labels = seq(from=3, to=12, by=2), limits = c(3,11.3)) +
  scale_y_continuous(breaks = seq(from=45, to=90, by=10), labels = seq(from=45, to=90, by=10), limits = c(45,85)) +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 10), size = 11, face = "bold", family = "Palatino"),
        axis.title.y = element_text(margin = margin(r = 10), size = 11, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 10, color = "black", family = "Palatino"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
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
        plot.title = element_text(family = "Palatino", face = "plain"),
        plot.margin = unit(c(0.1, 0.5, 0.1, -0.5), "cm")) + # top, right, bottom, left (0.5, 0.5, 0.1, -0.5)
  labs(x = "",
       y = "",
       tag ="Females")

cowplot::plot_grid(MaleBW, FemaleBW, MaleTL, FemaleTL, labels = c('A', '', 'B', ''), ncol = 2, nrow = 2, label_fontfamily = "Palatino")


ggsave("figures/weekly_phenotypes.tiff", height = 6, width = 6.5, compression = "lzw")
ggsave("figures/weekly_phenotypes.pdf", height = 6, width = 6.5)

dev.off()
