#!/usr/bin/env Rscript --vanilla

##############################################################

# Author: Mallory A. Ballinger

# This script plots *relative* extremity lengths of New York mice
# and Brazil mice across generations (i.e. common garden experiment #1).
# Data were cleaned using the script ./clean_Generations.R.
# This script generates Figure 2 in Ballinger_AmNat_2021.


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

GenerationMetaData <- read_csv(here("data/processed/GenerationColonyData.csv")) %>%
  select(-GUID, -Collector_ID, -Age_days, -Notes) %>% select(-1) %>%
  mutate(Sex = fct_relevel(Sex, "Male", "Female")) %>% # put males before females
  mutate(Population = fct_relevel(Population, "New York", "Brazil")) # put evolved cold pop first

# get sample size of each column
#colSums(!is.na(GenerationMetaData))





##############################################################
# Apply filtering and calculate residuals
##############################################################

# Based on outlier tests (see ./model_Generations.R), any tail length
# less than 50mm is an extreme outlier

Generation_filtered <- GenerationMetaData %>%
  mutate(Tail_Length_mm = ifelse(Tail_Length_mm < 50, NA, Tail_Length_mm)) %>%
  mutate(RelativeTail = (Tail_Length_mm) / (Body_Weight_g))


# Based on outlier tests (see 'code/model_Generations.R'), any ear length
# less than 8mm is an extreme outlier

Generation_filtered_2 <- GenerationMetaData %>%
  mutate(Ear_Length_mm = ifelse(Ear_Length_mm < 8, NA, Ear_Length_mm))  %>%
  mutate(RelativeEar = (Ear_Length_mm) / (Body_Weight_g))





##############################################################
# Get values from statistical analyses
##############################################################

# Refer to ./model_Generations_relative.R for model comparisons and statistical analyses

# > car::Anova(lm(rank(Body_Weight_g) ~ Sex * Population * Generation, data = GenerationMetaData), type = "III")
# Anova Table (Type III tests)
# 
# Response: rank(Body_Weight_g)
# Sum Sq  Df  F value    Pr(>F)    
# (Intercept)               16755268   1 1963.7874 < 2.2e-16 ***
# Sex                         433369   1   50.7926 4.243e-12 ***
# Population                 2410634   1  282.5364 < 2.2e-16 ***
# Generation                   62333   2    3.6528   0.02671 *  
# Sex:Population                4388   1    0.5143   0.47366    
# Sex:Generation               15273   2    0.8951   0.40933    
# Population:Generation        59838   2    3.5066   0.03084 *  
# Sex:Population:Generation    46731   2    2.7386   0.06577 .  
# Residuals                  3745600 439                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

BW_deets <- ("&#42;pop x gen<br>\
             &#42;sex<br>\
             &#42;pop<br>\
             &#42;gen")

# > car::Anova(lm(RelativeTail ~ Sex * Population * Generation,
#                 +            data = Generation_filtered), type = "III")
# Anova Table (Type III tests)
# 
# Response: RelativeTail
# Sum Sq  Df    F value    Pr(>F)    
# (Intercept)               6949.0   1 11236.7825 < 2.2e-16 ***
# Sex                         37.5   1    60.6451 5.394e-14 ***
# Population                 198.8   1   321.4267 < 2.2e-16 ***
# Generation                   8.8   2     7.0821 0.0009441 ***
# Sex:Population               0.0   1     0.0506 0.8222177    
# Sex:Generation               1.6   2     1.3322 0.2650213    
# Population:Generation        6.5   2     5.2860 0.0054042 ** 
# Sex:Population:Generation    4.9   2     3.9512 0.0199513 *  
# Residuals                  259.7 420                                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

TL_deets <- ("&#42;sex x pop x gen<br>
            &#42;pop x gen<br>\
            &#42;sex<br>\
            &#42;pop<br>\
            &#42;gen")


# > car::Anova(lm(RelativeEar ~ Sex * Population * Generation,
#                 +               data = Generation_filtered_2), type = "III")
# Anova Table (Type III tests)
# 
# Response: RelativeEar
# Sum Sq  Df    F value    Pr(>F)    
# (Intercept)               208.353   1 10098.5529 < 2.2e-16 ***
# Sex                         1.307   1    63.3558  1.61e-14 ***
# Population                  3.984   1   193.0768 < 2.2e-16 ***
# Generation                  0.202   2     4.8859 0.0079849 ** 
# Sex:Population              0.012   1     0.5765 0.4481293    
# Sex:Generation              0.047   2     1.1375 0.3215933    
# Population:Generation       0.352   2     8.5330 0.0002329 ***
# Sex:Population:Generation   0.092   2     2.2382 0.1079115    
# Residuals                   8.707 422                    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

EL_deets <- ("&#42;pop x gen<br>\
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
        plot.margin = unit(c(0, -0.25, 0, 0.45), "cm")) +
  labs(x = "Body Mass (g)",
       y = NULL,
       title = "Females")


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
                      jittered_points = TRUE, position = "raincloud",
                      alpha = 0.8, point_size = 1.85, point_alpha = 0.9, scale = 0.3,
                      point_shape=21, point_color = "white", color = NA) +
  geom_boxplot(aes(x = RelativeTail, y = Generation, color = Population),
               position = position_dodge(width = 0.01),  outlier.shape = NA,
               alpha = 0.5, width = 0.17) +
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
  labs(x = "Relative Tail Length",
       y = NULL,
       title = "Females")


Mtailridge <- 
  ggplot(data = MaleTL, aes(y = Generation, x = RelativeTail)) +
  geom_density_ridges(aes(point_color = Population, point_fill = Population, fill = Population),
                      jittered_points = TRUE, position = "raincloud",
                      alpha = 0.8, point_size = 1.85, point_alpha = 0.9, scale = 0.3,
                      point_shape=21, point_color = "white", color = NA) +
  geom_boxplot(aes(x = RelativeTail, y = Generation, color = Population),
               position = position_dodge(width = 0.01),  outlier.shape = NA,
               alpha = 0.5, width = 0.17, fill = "white") +
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

MaleEL <- Generation_filtered_2 %>%
  filter(Sex == "Male")

FemaleEL <- Generation_filtered_2 %>%
  filter(Sex == "Female")


Fearridge <- 
  ggplot(data = FemaleEL, aes(y = Generation, x = RelativeEar)) +
  geom_density_ridges(aes(point_color = Population, point_fill = Population, fill = Population),
                      jittered_points = TRUE, position = "raincloud",
                      alpha = 0.8, point_size = 1.85, point_alpha = 0.9, scale = 0.3,
                      point_shape=21, point_color = "white", color = NA) +
  geom_boxplot(aes(x = RelativeEar, y = Generation, color = Population),
               position = position_dodge(width = 0.01),  outlier.shape = NA,
               alpha = 0.5, width = 0.17) +
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
        plot.margin = unit(c(-0.1, -0.25, -0.25, 0.5), "cm")) +
  labs(x = "Relative Ear Length",
       y = "Generation",
       title = "Females")


Mearridge <- 
  ggplot(data = MaleEL, aes(y = Generation, x = RelativeEar)) +
  geom_density_ridges(aes(point_color = Population, point_fill = Population, fill = Population),
                      jittered_points = TRUE, position = "raincloud",
                      alpha = 0.8, point_size = 1.85, point_alpha = 0.9, scale = 0.3,
                      point_shape=21, point_color = "white", color = NA) +
  geom_boxplot(aes(x = RelativeEar, y = Generation, color = Population),
               position = position_dodge(width = 0.01),  outlier.shape = NA,
               alpha = 0.5, width = 0.17, fill = "white") +
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
        plot.tag.position = c(0.98,0.84),
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

ggsave("results/figures/Generations_relative.tiff", height = 7, width = 6, compression = "lzw")
ggsave("results/figures/Generations_relative.pdf", height = 7, width = 6)
