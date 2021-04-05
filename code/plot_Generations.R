#!/usr/bin/env Rscript --vanilla

##############################################################
# Author: Mallory A. Ballinger
# Script first created: 24-Feb-2021
# Script last updated:  03-Apr-2021


# This script plots sex-specific body weight and extremity lengths of New York mice
# and Brazil mice cross generations.
# This script generates Figure 2 in Ballinger_et_al_2021_AmNat.


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

GenerationMetaData <- read_csv(here("data/processed/GenerationColonyData.csv")) %>%
  select(-GUID, -Collector_ID, -Age_days, -Notes) %>%
  mutate(Ear_Length_mm = as.numeric(Ear_Length_mm),
         Hindfoot_Length_mm = as.numeric(Hindfoot_Length_mm),
         Tail_Length_mm = as.numeric(Tail_Length_mm),
         Body_Length_mm = as.numeric(Body_Length_mm),
         Body_Weight_g = as.numeric(Body_Weight_g)) %>%
  mutate(Population = fct_recode(Population, "New York" = "New_York")) %>%
  mutate(Sex = fct_relevel(Sex, "Male", "Female")) %>% # puts males before females
  mutate(Population = fct_relevel(Population, "New York", "Brazil"))

# get sample size of each column
colSums(!is.na(GenerationMetaData))





##############################################################
# Apply filtering and calculate residuals
##############################################################

# Based on outlier tests (see 'code/model_Generations.R'), any tail length
# less than 50mm is an extreme outlier
Generation_filtered <- GenerationMetaData %>%
  mutate(Tail_Length_mm = ifelse(Tail_Length_mm < 50, NA, Tail_Length_mm))

# save TL x BW residuals
residsTLBW <- lm(Tail_Length_mm ~ Body_Weight_g, data = Generation_filtered,
                 na.action = na.exclude)
Generation_filtered$Resids_TLBW <- resid(residsTLBW)


# Based on outlier tests (see 'code/model_Generations.R'), any ear length
# less than 8mm is an extreme outlier
Generation_filtered_2 <- GenerationMetaData %>%
  mutate(Ear_Length_mm = ifelse(Ear_Length_mm < 8, NA, Ear_Length_mm))

# save TL x BW residuals
residsELBW <- lm(Ear_Length_mm ~ Body_Weight_g, data = Generation_filtered_2,
                 na.action = na.exclude)
Generation_filtered_2$Resids_ELBW <- resid(residsELBW)





##############################################################
# Get values from statistical analyses
##############################################################
# Refer to 'code/model_Generations.R' for model comparisons and statistical analyses

## car::Anova(lm(rank(Body_Weight_g) ~ Generation * Population * Sex, data = GenerationMetaData), type = "III")
# Anova Table (Type III tests)
# 
# Response: rank(Body_Weight_g)
# Sum Sq  Df F value    Pr(>F)    
# (Intercept)                140198   1 16.4317 5.966e-05 ***
#   Generation                  25753   2  1.5092   0.22223    
# Population                 234928   1 27.5345 2.407e-07 ***
#   Sex                          8415   1  0.9862   0.32121    
# Generation:Population        4862   2  0.2849   0.75221    
# Generation:Sex              23473   2  1.3756   0.25378    
# Population:Sex              33427   1  3.9177   0.04840 *  
#   Generation:Population:Sex   46731   2  2.7386   0.06577 .  
# Residuals                 3745600 439                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Body_Mass <- glue("****_pop_<br>\\
                  **_pop x sex_")

## car::Anova(lm(Tail_Length_mm ~ Body_Weight_g + Generation * Population * Sex, data = Generation_filtered), type = "III")
# Anova Table (Type III tests)
# 
# Response: Tail_Length_mm
# Sum Sq  Df   F value    Pr(>F)    
# (Intercept)                73475   1 3328.1354 < 2.2e-16 ***
#   Body_Weight_g                868   1   39.3297 8.922e-10 ***
#   Generation                    82   2    1.8514   0.15831    
# Population                    43   1    1.9400   0.16440    
# Sex                           13   1    0.5932   0.44161    
# Generation:Population        650   2   14.7101 6.696e-07 ***
#   Generation:Sex                12   2    0.2814   0.75488    
# Population:Sex                90   1    4.0670   0.04437 *  
#   Generation:Population:Sex    104   2    2.3548   0.09617 .  
# Residuals                   9250 419                        
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

TailLength <- glue("****_gen x pop_<br>\\
                  **_pop x sex_")


## car::Anova(lm(rank(Ear_Length_mm) ~ Body_Weight_g + Generation * Population * Sex, data = Generation_filtered_2), type = "III")
# Anova Table (Type III tests)
# 
# Response: rank(Ear_Length_mm)
# Sum Sq  Df F value    Pr(>F)    
# (Intercept)                 64210   1  5.0614  0.024972 *  
#   Body_Weight_g              310068   1 24.4414 1.102e-06 ***
#   Generation                 141698   2  5.5848  0.004033 ** 
#   Population                   1409   1  0.1110  0.739140    
# Sex                          7493   1  0.5907  0.442588    
# Generation:Population       47007   2  1.8527  0.158068    
# Generation:Sex              12208   2  0.4811  0.618411    
# Population:Sex                179   1  0.0141  0.905532    
# Generation:Population:Sex    2063   2  0.0813  0.921914    
# Residuals                 5429675 428                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

EarLength <- glue("**_gen_")





##############################################################
# Plot sex-specific body mass ridgeplot across generations
##############################################################

MaleBW <- GenerationMetaData %>%
  filter(Sex == "Male")

FemaleBW <- GenerationMetaData %>%
  filter(Sex == "Female")


Mmassridge <- 
  ggplot(data = MaleBW, aes(y = Generation, x = Body_Weight_g)) +
  geom_density_ridges(aes(point_color = Population, point_fill = Population, fill = Population),
                      jittered_points = TRUE, position = "raincloud",
                      alpha = 0.8, point_size = 1.85, point_alpha = 0.9, scale = 0.3,
                      point_shape=21, point_color = "white", color = NA) +
  geom_boxplot(aes(x = Body_Weight_g, y = Generation, color = Population),
               position = position_dodge(width = 0.01),  outlier.shape = NA,
               alpha = 0.5, width = 0.17) +
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
        axis.title.y = element_text(margin = margin(r = 5), size = 10, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        legend.position = c(0.025, 0.95),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(0.3, "cm"),
        #legend.key.height = unit(0.5, "cm"),
        #legend.key.width = unit(0.5, "cm"),
        legend.text = element_text(size=7, family = "Palatino"),
        legend.title = element_blank(),
        plot.tag = element_text(family = "Palatino", size = 10, face = "italic", hjust = 1),
        plot.tag.position = c(0.15,1.025),
        plot.title = element_text(size = 8, face = "italic", hjust = 0.05, vjust = -3, family = "Palatino"),
        plot.title.position = "panel",
        plot.margin = unit(c(0, -0.25, 0, 0.5), "cm")) +
  labs(x = "Body Mass (g)",
       y = NULL,
       title = "Males")
       #tag = Body_Mass)


Fmassridge <- 
  ggplot(data = FemaleBW, aes(y = Generation, x = Body_Weight_g)) +
  geom_density_ridges(aes(point_color = Population, point_fill = Population, fill = Population),
                      jittered_points = TRUE, position = "raincloud",
                      alpha = 0.8, point_size = 1.85, point_alpha = 0.9, scale = 0.3,
                      point_shape=21, point_color = "white", color = NA) +
  geom_boxplot(aes(x = Body_Weight_g, y = Generation, color = Population),
               position = position_dodge(width = 0.01),  outlier.shape = NA,
               alpha = 0.5, width = 0.17) +
  #annotate("text", x = 44, y=3.4, label="**gen x pop\n*pop x sex", family = "Palatino", fontface = 3, size = 3, hjust = 1) +
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
        plot.tag = element_markdown(family = "Palatino", size = 7, face = "italic", hjust = 1),
        plot.tag.position = c(0.98,0.83),
        plot.title = element_text(size = 8, face = "italic", hjust = 0.05, vjust = -3, family = "Palatino"),
        plot.title.position = "panel",
        plot.margin = unit(c(0, 0.75, 0.5, 0), "cm")) + # top, right, bottom, lef
  labs(x = NULL,
       y = NULL,
       title = "Females",
       tag = Body_Mass)


BW <- cowplot::plot_grid(Mmassridge, Fmassridge, labels = c('A)', ''), align = "h",
                         label_fontfamily = "Palatino", label_size = 12, label_x = 0.05, hjust = 0)





##############################################################
# Plot sex-specific tail length ridgeplot across generaitons
##############################################################

MaleTL <- Generation_filtered %>%
  filter(Sex == "Male")

FemaleTL <- Generation_filtered %>%
  filter(Sex == "Female")


Mtailridge <- 
  ggplot(data = MaleTL, aes(y = Generation, x = Resids_TLBW)) +
  geom_density_ridges(aes(point_color = Population, point_fill = Population, fill = Population),
                      jittered_points = TRUE, position = "raincloud",
                      alpha = 0.8, point_size = 1.85, point_alpha = 0.9, scale = 0.3,
                      point_shape=21, point_color = "white", color = NA) +
  geom_boxplot(aes(x = Resids_TLBW, y = Generation, color = Population),
               position = position_dodge(width = 0.01),  outlier.shape = NA,
               alpha = 0.5, width = 0.17) +
  scale_x_continuous(breaks = seq(from=-20, to=20, by=10),
                     labels = seq(from=-20, to=20, by=10),
                     limits = c(-20,20)) +
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
        axis.title.x = element_text(margin = margin(t = 10), size = 10, face = "bold", family = "Palatino", hjust = 1.15),
        axis.title.y = element_text(margin = margin(r = 3), size = 9, face = "bold", family = "Palatino"),
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
        plot.margin = unit(c(0, -0.25, 0, 0.375), "cm")) +
  labs(x = "Tail Length (resids)",
       y = NULL,
       title = "Males")
       #tag = "Males")


Ftailridge <- 
  ggplot(data = FemaleTL, aes(y = Generation, x = Resids_TLBW)) +
  geom_density_ridges(aes(point_color = Population, point_fill = Population, fill = Population),
                      jittered_points = TRUE, position = "raincloud",
                      alpha = 0.8, point_size = 1.85, point_alpha = 0.9, scale = 0.3,
                      point_shape=21, point_color = "white", color = NA) +
  geom_boxplot(aes(x = Resids_TLBW, y = Generation, color = Population),
               position = position_dodge(width = 0.01),  outlier.shape = NA,
               alpha = 0.5, width = 0.17, fill = "white") +
  #annotate("text", x = 19, y=3, label="**gen\n**gen x sex", family = "Palatino", fontface = 3, size = 3, hjust = 0) +
  scale_x_continuous(breaks = seq(from=-20, to=20, by=10),
                     labels = seq(from=-20, to=20, by=10),
                     limits = c(-20,20)) +
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
        axis.title.x = element_text(margin = margin(t = 10), size = 10, face = "bold", family = "Palatino", hjust = 0.6),
        axis.title.y = element_text(margin = margin(r = 10), size = 10, face = "bold", family = "Palatino"),
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
        plot.tag = element_markdown(family = "Palatino", size = 7, face = "italic", hjust = 1),
        plot.tag.position = c(0.98,0.83),
        plot.title = element_text(size = 8, face = "italic", hjust = 0.05, vjust = -3, family = "Palatino"),
        plot.title.position = "panel",
        plot.margin = unit(c(0, 0.75, 0.5, 0), "cm")) + # top, right, bottom, left (0.5, 0.5, 0.1, -0.5)
  labs(x = NULL,
       y = NULL,
       title = "Females",
       tag = TailLength)


Tails <- cowplot::plot_grid(Mtailridge, Ftailridge, labels = c('B)', ''), align = "h",
                            label_fontfamily = "Palatino", label_size = 12, label_x = 0.05, hjust = 0)





##############################################################
# Plot sex-specific ear length ridgeplot across generaitons
##############################################################

MaleEL <- Generation_filtered_2 %>%
  filter(Sex == "Male")

FemaleEL <- Generation_filtered_2 %>%
  filter(Sex == "Female")


Mearridge <- 
  ggplot(data = MaleEL, aes(y = Generation, x = Resids_ELBW)) +
  geom_density_ridges(aes(point_color = Population, point_fill = Population, fill = Population),
                      jittered_points = TRUE, position = "raincloud",
                      alpha = 0.8, point_size = 1.85, point_alpha = 0.9, scale = 0.3,
                      point_shape=21, point_color = "white", color = NA) +
  geom_boxplot(aes(x = Resids_ELBW, y = Generation, color = Population),
               position = position_dodge(width = 0.01),  outlier.shape = NA,
               alpha = 0.5, width = 0.17) +
  scale_x_continuous(breaks = seq(from=-5, to=10, by=5),
                     labels = seq(from=-5, to=10, by=5),
                     limits = c(-5,7)) +
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
        axis.title.x = element_text(margin = margin(t = 12), size = 10, face = "bold", family = "Palatino", hjust = 1.2),
        axis.title.y = element_text(margin = margin(r = 3), size = 9, face = "bold", family = "Palatino"),
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
        plot.margin = unit(c(-0.1, -0.25, -.250, 0.5), "cm")) +
  labs(x = "Ear Length (resids)",
       y = "Generation",
       title = "Males")
       #tag = "Males")


Fearridge <- 
  ggplot(data = FemaleEL, aes(y = Generation, x = Resids_ELBW)) +
  geom_density_ridges(aes(point_color = Population, point_fill = Population, fill = Population),
                      jittered_points = TRUE, position = "raincloud",
                      alpha = 0.8, point_size = 1.85, point_alpha = 0.9, scale = 0.3,
                      point_shape=21, point_color = "white", color = NA) +
  geom_boxplot(aes(x = Resids_ELBW, y = Generation, color = Population),
               position = position_dodge(width = 0.01),  outlier.shape = NA,
               alpha = 0.5, width = 0.17, fill = "white") +
  #annotate("text", x = 19, y=3, label="**gen\n**gen x sex", family = "Palatino", fontface = 3, size = 3, hjust = 0) +
  scale_x_continuous(breaks = seq(from=-5, to=10, by=5),
                     labels = seq(from=-5, to=10, by=5),
                     limits = c(-5,7)) +
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
        axis.title.x = element_text(margin = margin(t = 10), size = 10, face = "bold", family = "Palatino", hjust = 0.6),
        axis.title.y = element_text(margin = margin(r = 10), size = 9, face = "bold", family = "Palatino"),
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
        plot.tag = element_markdown(family = "Palatino", size = 7, face = "italic", hjust = 1),
        plot.tag.position = c(0.98,0.85),
        plot.title = element_text(size = 8, face = "italic", hjust = 0.05, vjust = -3, family = "Palatino"),
        plot.title.position = "panel",
        plot.margin = unit(c(-0.1, 0.75, -0.25, 0), "cm")) + # top, right, bottom, left (0.5, 0.5, 0.1, -0.5)
  labs(x = NULL,
       y = NULL,
       title = "Females",
       tag = EarLength)


Ears <- cowplot::plot_grid(Mearridge, Fearridge, labels = c('C)', ''), align = 'h',
                           label_fontfamily = "Palatino", label_size = 12, label_x = 0.05, hjust = 0)



cowplot::plot_grid(BW, Tails, Ears, ncol=1, nrow=3, label_fontfamily = "Palatino",
                   label_size = 12, label_x = 0.05, hjust = 0, align = 'hv')

ggsave("results/figures/Generations_colony.tiff", height = 7, width = 6, compression = "lzw")
ggsave("results/figures/Generations_colony.pdf", height = 7, width = 6)
