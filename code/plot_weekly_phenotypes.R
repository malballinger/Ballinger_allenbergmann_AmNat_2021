#!/usr/bin/env Rscript --vanilla

# name: weekly_phenotypes_graphed.R
#
# Plot evolved and plastic differences in weekly body weights and tail lengths
#
# input:
#       - data/raw/WeeklyMetaData
# output:
#       - results/figures/WeeklyPhenotypes

# Clear R environment and load libraries
rm(list = ls())
library(tidyverse)
library(here)
library(cowplot)


# Read in data
WeeklyMetaData <- read_csv(here("data/raw/WeeklyMetaData.csv")) %>%
  select(-DOB, -DateMeasured) %>%
  filter(Population == "BRAZIL" | Population == "NEW_YORK") %>%
  #filter(Age_weeks > 3) %>%  #NY lines sometimes were weaned & measured slightly before 3 weeks
  filter(Age_weeks < 12) %>%
  mutate(Sex = fct_recode(Sex, "Females" = "F", "Males" = "M")) %>% # spells out males and females
  mutate(Environment = fct_relevel(Environment,
                                   "RT", "COLD" )) %>% # puts RT before COLD
  mutate(Sex = fct_relevel(Sex, "Males", "Females")) # puts males before females
  
# Clean data of any weird spaces, etc.
WeeklyMetaData[] <- lapply(WeeklyMetaData, function(x) if(is.factor(x)) factor(x) else x)


#sex_labels <- c("Females", "Males")
#names(sex_labels) <- c("F", "M")

BodyWeight <-
  ggplot(data=WeeklyMetaData, aes(x=Age_weeks, y= BodyWeight_g, color=Population)) +
  geom_smooth(aes(linetype=Environment, color=Population), size=1, method="loess") + #std error
  geom_point(position = position_dodge(0.1), size=1.5, alpha=0.5) +
  scale_color_manual(values=c("goldenrod1","dodgerblue4")) +
  coord_cartesian(xlim = c(3,12), ylim = c(3,25)) +
  facet_wrap(facets = "Sex",
             #strip.position = "top",
             scales = "fixed") +
             #labeller=labeller(Sex = sex_labels)) +
  theme_half_open(12) +
  panel_border() +
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 20), size = 15, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 20), size = 15, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold", color = "black"),
        axis.text.y = element_text(size = 12, face = "bold", color = "black"),
        #legend.title = element_blank(),
        legend.position = "none",
       # legend.key.size = unit(1.5, "cm"),
       # legend.text = element_text(size=15),
        #legend.key = element_rect(fill = "transparent"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
  labs(x = "Age (weeks)", y = "Body Weight (g)")
BodyWeight

TailLength <-
  ggplot(data=WeeklyMetaData, aes(x=Age_weeks, y=TailLength_mm, color=Population)) +
  geom_smooth(aes(linetype=Environment, color=Population), size=1, method="loess") + #std error
  geom_point(position = position_dodge(0.1), size=1.5, alpha=0.5) +
  scale_color_manual(values=c("goldenrod1","dodgerblue4")) +
  coord_cartesian(xlim = c(3,12), ylim = c(45,85)) +
  facet_grid(. ~ Sex) +
  theme_half_open(12) +
  panel_border() +
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 20), size = 15, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 20), size = 15, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold", color = "black"),
        axis.text.y = element_text(size = 12, face = "bold", color = "black"),
        #legend.title = element_blank(),
        legend.position = "none",
        # legend.key.size = unit(1.5, "cm"),
        # legend.text = element_text(size=15),
        #legend.key = element_rect(fill = "transparent"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
  labs(x = "Age (weeks)", y = "Tail Length (mm)")
TailLength
plot_grid(BodyWeight, TailLength, labels = c('A', 'B'), ncol = 2, nrow = 2)


#### Load PostDissection datasheet ####
# Read in data
Development <- WeeklyMetaData %>%
  filter(Age_category == "weaning" | Age_category == "endexp") %>%
  mutate(PopEnv = paste(Population, Environment)) %>%
  mutate(PopEnv = fct_relevel(PopEnv, "BRAZIL RT", "BRAZIL COLD", "NEW_YORK RT", "NEW_YORK COLD")) %>%
  mutate(Age_category = fct_relevel(Age_category, "weaning", "endexp" )) # puts weaning before adult

WeaningBW <-
  ggplot(data=Development, aes(x = Age_category, y = BodyWeight_g, fill = Population)) +
  geom_boxplot(position = position_dodge(width=0.05), alpha=0.65, outlier.shape = NA) +
  geom_dotplot(binaxis='y', stackdir = 'center', position = position_dodge(0.1)) +
  #geom_jitter(position = position_dodge(0.8), alpha=2, size=3, shape=21) +
  stat_summary(
    fun = median,
    geom = 'line',
    aes(group = PopEnv, colour = Population, linetype=Environment),
    position = "identity", size=1.5, alpha=0.8) +
  scale_color_manual(values=c("goldenrod1","dodgerblue4")) +
  scale_fill_manual(values=c("goldenrod1","dodgerblue4")) +
  facet_wrap(.~Sex) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.y = element_text(margin = margin(r = 20), size = 15, face = "bold"),
        axis.text.x = element_text(size = 15, face = "bold", color = "black"),
        axis.text.y = element_text(size = 12, face = "bold", color = "black")) +
  theme(strip.text = element_text(size = 10, face = "bold", color = "black")) +
  scale_x_discrete(labels=c("Weaning", "Adult")) +
  theme(legend.title = element_blank(),
        legend.position="bottom", legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size=5),
        legend.key = element_rect(fill = "transparent")) +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
  labs(x = "Age", y = "Body Weight (g)")
WeaningBW

WeaningTL <-
  ggplot(data=Development, aes(x = Age_category, y = TailLength_mm, fill = PopEnv)) +
  geom_boxplot(position = position_dodge(width=0.05), alpha=0.65, outlier.shape = NA) +
  geom_dotplot(binaxis='y', stackdir = 'center', position = position_dodge(0.1)) +
  #geom_jitter(position = position_dodge(0.8), alpha=2, size=3, shape=21) +
  stat_summary(
    fun = median,
    geom = 'line',
    aes(group = PopEnv, colour = PopEnv, linetype=Environment),
    position = "identity", size=1.5, alpha=0.8) +
  scale_color_manual(values=c("goldenrod4","goldenrod1", "dodgerblue4", "dodgerblue1")) +
  scale_fill_manual(values=c("goldenrod4","goldenrod1", "dodgerblue4", "dodgerblue1")) +
  facet_wrap(.~Sex) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.y = element_text(margin = margin(r = 20), size = 15, face = "bold"),
        axis.text.x = element_text(size = 15, face = "bold", color = "black"),
        axis.text.y = element_text(size = 12, face = "bold", color = "black")) +
  theme(strip.text = element_text(size = 10, face = "bold", color = "black")) +
  scale_x_discrete(labels=c("Weaning", "Adult")) +
  theme(legend.title = element_blank(),
        legend.position="bottom", legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size=5),
        legend.key = element_rect(fill = "transparent")) +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
  labs(x = "Age", y = "Tail Length (mm)")
WeaningTL

WeaningTL2 <-
  ggplot(data=Development, aes(x = Age_category, y = TailLength_mm, fill = Population)) +
  geom_boxplot(position = position_dodge(width=0.05), alpha=0.65, outlier.shape = NA) +
  geom_dotplot(binaxis='y', stackdir = 'center', position = position_dodge(0.1)) +
  #geom_jitter(position = position_dodge(0.8), alpha=2, size=3, shape=21) +
  stat_summary(
    fun = median,
    geom = 'line',
    aes(group = PopEnv, colour = Population, linetype=Environment),
    position = "identity", size=1.5, alpha=0.8) +
  scale_color_manual(values=c("goldenrod4","goldenrod1", "dodgerblue4", "dodgerblue1")) +
  scale_fill_manual(values=c("goldenrod4","goldenrod1", "dodgerblue4", "dodgerblue1")) +
  facet_wrap(.~Sex) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.y = element_text(margin = margin(r = 20), size = 15, face = "bold"),
        axis.text.x = element_text(size = 15, face = "bold", color = "black"),
        axis.text.y = element_text(size = 12, face = "bold", color = "black")) +
  theme(strip.text = element_text(size = 10, face = "bold", color = "black")) +
  scale_x_discrete(labels=c("Weaning", "Adult")) +
  theme(legend.title = element_blank(),
        legend.position="bottom", legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size=5),
        legend.key = element_rect(fill = "transparent")) +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
  labs(x = "Age", y = "Tail Length (mm)")
WeaningTL2