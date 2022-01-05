#!/usr/bin/env Rscript --vanilla

################################################################################

# Author: Mallory A. Ballinger

# This script plots heritability (via parent-offspring regressions) for body weight,
# relative tail length, and relative ear length for both Brazil and New York colonies.
# This script generates Figure 2 and Figure S3 in Ballinger_AmNat_2021.


################################################################################
# Required packages
################################################################################

rm(list = ls()) # clear R's environment

library(tidyverse)
library(here)
library(glue)
library(xtable)
library(magrittr)
library(ggtext)
library(cowplot)

source("code/model_N2N3Heritability.R") # call heritabilities that were previously generated


################################################################################
# Plot heritabilities (via midparent-offspring regressions)
################################################################################

# Body Weight

midparentsBW <- midparent_values %>%
  select(Population, Generation, Midparent_ID, Parental_Midpoint_BW)

offspring_meanBW <- h2_filtered %>%
  filter(Generation == "N3") %>%
  select(Population, Generation, Midparent_ID, Body_Weight_g) %>%
  rename("Offspring_BW" = "Body_Weight_g")

midparents_offspringBW <- inner_join(midparentsBW, offspring_meanBW, by = "Midparent_ID") %>%
  mutate(Population.x = recode_factor(Population.x, "New York" = glue("New York (*h^2* = {NY_parents_offspring_BW_h2} &#177; {NY_parents_offspring_BW_se})"),
                                      "Brazil" = glue("Brazil (*h^2* = {BZ_parents_offspring_BW_h2} &#177; {BZ_parents_offspring_BW_se})*")))
  

BW_midparent <- 
  ggplot(data = midparents_offspringBW, aes(x=Parental_Midpoint_BW, y=Offspring_BW, fill = Population.x)) +
  geom_line(aes(color = Population.x), stat = "smooth", method = "lm", alpha = 0.8, se = FALSE, size = 1, show.legend = TRUE) +
  geom_point(shape = 21, size=2, alpha = 0.8, color = "white") +
  scale_color_manual(values=c("dodgerblue4", "goldenrod1")) +
  scale_fill_manual(values=c("dodgerblue4", "goldenrod1")) +
  scale_y_continuous(limits = c(10,40)) +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 3), size = 10, family = "Palatino"),
        axis.title.y = element_markdown(margin = margin(r = 4), size = 10, family = "Palatino"),
        axis.text.x = element_text(size = 9, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 9, color = "black", family = "Palatino"),
        legend.position = c(0.345, 0.95),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(3, "mm"),
        legend.spacing.x = unit(0.5, "mm"),
        legend.spacing.y = unit(0.5, "mm"),
        legend.text = element_markdown(size=8, family = "Palatino"),
        legend.title = element_blank()) +
  labs(x = "Midparent Body Mass",
       y = "Offspring Body Mass")


# Tail Length

midparentsTL <- midparent_values %>%
  select(Population, Generation, Midparent_ID, Parental_Midpoint_TL)

offspring_meanTL <- h2_filtered %>%
  filter(Generation == "N3") %>%
  select(Population, Generation, Midparent_ID, RelativeTail) %>%
  rename("Offspring_TL" = "RelativeTail")

midparents_offspringTL <- inner_join(midparentsTL, offspring_meanTL, by = "Midparent_ID") %>%
  mutate(Population.x = recode_factor(Population.x, "New York" = glue("New York (*h^2* = {NY_parents_offspring_RelTL_h2} &#177; {NY_parents_offspring_RelTL_se})*"),
                                      "Brazil" = glue("Brazil (*h^2* = {BZ_parents_offspring_RelTL_h2} &#177; {BZ_parents_offspring_RelTL_se})*")))

TL_midparent <- 
  ggplot(data = midparents_offspringTL, aes(x=Parental_Midpoint_TL, y=Offspring_TL,
                                            fill = Population.x)) +
  geom_line(aes(color = Population.x), stat = "smooth", method = "lm", alpha = 0.8, se = FALSE, size = 1, show.legend = TRUE) +
  geom_point(shape = 21, size=2, alpha = 0.8, color = "white") +
  scale_color_manual(values=c("dodgerblue4", "goldenrod1")) +
  scale_fill_manual(values=c("dodgerblue4", "goldenrod1")) +
  scale_y_continuous(limits = c(2,6)) +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 2.5), size = 10, family = "Palatino"),
        axis.title.y = element_markdown(margin = margin(r = 4), size = 10, family = "Palatino"),
        axis.text.x = element_text(size = 9, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 9, color = "black", family = "Palatino"),
        legend.position = c(0.355, 0.95),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(3, "mm"),
        legend.spacing.x = unit(0.5, "mm"),
        legend.spacing.y = unit(0.5, "mm"),
        legend.text = element_markdown(size=8, family = "Palatino"),
        legend.title = element_blank()) +
  labs(x = "Midparent Relative Tail Length",
       y = "Offspring Relative Tail Length")


# Ear Length

midparentsEL <- midparent_values %>%
  select(Population, Generation, Midparent_ID, Parental_Midpoint_EL)

offspring_meanEL <- h2_filtered %>%
  filter(Generation == "N3") %>%
  select(Population, Generation, Midparent_ID, RelativeEar) %>%
  rename("Offspring_EL" = "RelativeEar")

midparents_offspringEL <- inner_join(midparentsEL, offspring_meanEL, by = "Midparent_ID") %>%
  mutate(Population.x = recode_factor(Population.x, "New York" = glue("New York (*h^2* = {NY_parents_offspring_RelEL_h2} &#177; {NY_parents_offspring_RelEL_se})"),
                                      "Brazil" = glue("Brazil (*h^2* = {BZ_parents_offspring_RelEL_h2} &#177; {BZ_parents_offspring_RelEL_se})*")))

EL_midparent <- 
  ggplot(data = midparents_offspringEL, aes(x=Parental_Midpoint_EL, y=Offspring_EL,
                                            fill = Population.x)) +
  geom_line(aes(color = Population.x), stat = "smooth", method = "lm", alpha = 0.8, se = FALSE, size = 1, show.legend = TRUE) +
  geom_point(shape = 21, size=2, alpha = 0.8, color = "white") +
  scale_color_manual(values=c("dodgerblue4", "goldenrod1")) +
  scale_fill_manual(values=c("dodgerblue4", "goldenrod1")) +
  scale_y_continuous(limits = c(0.3,1)) +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 3), size = 10, family = "Palatino"),
        axis.title.y = element_markdown(margin = margin(r = 4), size = 10, family = "Palatino"),
        axis.text.x = element_text(size = 9, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 9, color = "black", family = "Palatino"),
        legend.position = c(0.36, 0.95),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(3, "mm"),
        legend.spacing.x = unit(0.5, "mm"),
        legend.spacing.y = unit(0.5, "mm"),
        legend.text = element_markdown(size=8, family = "Palatino"),
        legend.title = element_blank()) +
  labs(x = "Midparent Relative Ear Length",
       y = "Offspring Relative Ear Length")


midparent_h2 <- cowplot::plot_grid(BW_midparent, NULL, TL_midparent, NULL, EL_midparent,
                                   labels = c('A)', '', 'B)', '', 'C)'), ncol = 5, nrow = 1,
                                   rel_widths = c(1, 0.02, 1, 0.02, 1), align = "h",
                                   label_fontfamily = "Palatino", label_size = 14,
                                   label_x = 0, hjust = 0, axis = "bt")


#ggsave("results/figures/N2N3_h2.tiff", height = 4, width = 8.5, compression = "lzw")
ggsave("results/figures/N2N3_h2.pdf", height = 3.6, width = 8)


################################################################################
# Plot sex-specific parent-offspring slopes
################################################################################

# Body Weight

parents_BW <- h2_filtered %>%
  filter(Generation == "N2") %>%
  select(Population, Generation, Midparent_ID, Body_Weight_g, Sex) %>%
  rename("Parental_Generation" = "Generation",
         "Parental_Population" = "Population",
         "Parental_Sex" = "Sex",
         "Parent_BW" = "Body_Weight_g")

Offspring_BW <- h2_filtered %>%
  filter(Generation == "N3") %>%
  select(Population, Sex, Generation, Midparent_ID, Body_Weight_g) %>%
  rename("Offspring_Generation" = "Generation",
         "Offspring_BW" = "Body_Weight_g",
         "Offspring_Population" = "Population",
         "Offspring_Sex" = "Sex")

parents_offspring_BW <- inner_join(parents_BW, Offspring_BW, by = "Midparent_ID") %>%
  mutate(Parental_Sex = recode_factor(Parental_Sex, "Female" = "Mothers' Body Mass",
                                      "Male" = "Fathers' Body Mass"))

parents_offspring_BW_plot <-
  ggplot(data = parents_offspring_BW, aes(x=Parent_BW, y = Offspring_BW, color = Parental_Population)) +
  geom_line(stat = "smooth", method = "lm", alpha = 0.8, size = 1, show.legend = TRUE) +
  geom_point(na.rm = FALSE, alpha = 0.8) +
  scale_color_manual(values=c("dodgerblue4", "goldenrod1"),
                     breaks=c("New York","Brazil")) +
  scale_y_continuous(limits = c(10,40)) +
  facet_grid(~ Parental_Sex, switch = "x" , scales = "free_x") +
  theme_half_open(10) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 5), size = 8, family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(margin = margin(0,0,0.15,0, "cm"), size = 8, family = "Palatino"),
        legend.position = 'none') +
  labs(x = NULL,
       y = "Offspring Body Mass")


# bw <- cowplot::plot_grid(BW_midparent, NULL, parents_offspring_BW_plot, labels = c('A)', '', 'B)'), align = "h",
#                          axis = "bt", rel_widths = c(1, 0.05, 1.2), ncol = 3, nrow = 1,
#                          label_fontfamily = "Palatino", label_size = 12, label_x = 0.05, hjust = 0.8)


# Tail Length

Parent_TL <- h2_filtered %>%
  filter(Generation == "N2") %>%
  select(Population, Generation, Midparent_ID, RelativeTail, Sex) %>%
  rename("Parental_Generation" = "Generation",
         "Parental_Population" = "Population",
         "Parental_Sex" = "Sex",
         "Parent_TL" = "RelativeTail")

Offspring_TL <- h2_filtered %>%
  filter(Generation == "N3") %>%
  select(Population, Sex, Generation, Midparent_ID, RelativeTail) %>%
  rename("Offspring_Generation" = "Generation",
         "Offspring_TL" = "RelativeTail",
         "Offspring_Population" = "Population",
         "Offspring_Sex" = "Sex")

parent_offspring_TL <- inner_join(Parent_TL, Offspring_TL, by = "Midparent_ID") %>%
  mutate(Parental_Sex = recode_factor(Parental_Sex, "Female" = "Mothers' Rel. Tail Length",
                                      "Male" = "Fathers' Rel. Tail Length"))

parent_offspring_TL_plot <-
  ggplot(data = parent_offspring_TL, aes(x=Parent_TL, y = Offspring_TL, color = Parental_Population)) +
  geom_line(stat = "smooth", method = "lm", alpha = 0.8, size = 1, show.legend = TRUE) +
  geom_point(na.rm = FALSE, alpha = 0.8) +
  scale_color_manual(values=c("dodgerblue4", "goldenrod1"),
                     breaks=c("New York","Brazil")) +
  scale_y_continuous(limits = c(2,6)) +
  facet_grid(~ Parental_Sex, switch = "x" , scales = "free_x") +
  theme_half_open(10) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 5), size = 8, family = "Palatino"),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(margin = margin(0,0,0.15,0, "cm"), size = 8, family = "Palatino"),
        legend.position = 'none') +
  labs(x = "",
       y = "Offspring Relative Tail Length")


# tl <- cowplot::plot_grid(TL_midparent, NULL, parent_offspring_TL_plot, labels = c('C)', '', 'D)'), align = "h",
#                          axis = "bt", rel_widths = c(1, 0.05, 1.2), ncol = 3, nrow = 1,
#                          label_fontfamily = "Palatino", label_size = 12, label_x = 0.05, hjust = 0.8)


# Ear Length

Parent_EL <- h2_filtered %>%
  filter(Generation == "N2") %>%
  select(Population, Generation, Midparent_ID, RelativeEar, Sex) %>%
  rename("Parental_Generation" = "Generation",
         "Parental_Population" = "Population",
         "Parental_Sex" = "Sex",
         "Parent_EL" = "RelativeEar")

Offspring_EL <- h2_filtered %>%
  filter(Generation == "N3") %>%
  select(Population, Sex, Generation, Midparent_ID, RelativeEar) %>%
  rename("Offspring_Generation" = "Generation",
         "Offspring_EL" = "RelativeEar",
         "Offspring_Population" = "Population",
         "Offspring_Sex" = "Sex")

parent_offspring_EL <- inner_join(Parent_EL, Offspring_EL, by = "Midparent_ID") %>%
  mutate(Parental_Sex = recode_factor(Parental_Sex, "Female" = "Mothers' Rel. Ear Length",
                                      "Male" = "Fathers' Rel. Ear Length"))

parent_offspring_EL_plot <-
  ggplot(data = parent_offspring_EL, aes(x=Parent_EL, y = Offspring_EL, color = Parental_Population)) +
  geom_line(stat = "smooth", method = "lm", alpha = 0.8, size = 1, show.legend = TRUE) +
  geom_point(na.rm = FALSE, alpha = 0.8) +
  scale_color_manual(values=c("dodgerblue4", "goldenrod1"),
                     breaks=c("New York","Brazil")) +
  scale_y_continuous(limits = c(0.3,1.05)) +
  facet_wrap(~ Parental_Sex, strip.position = "bottom", scales = "free_x") +
  theme_half_open(10) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(r = 5), size = 8, family = "Palatino"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 8, color = "black", family = "Palatino"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(margin = margin(0,0,0.15,0, "cm"), size = 8, family = "Palatino"),
        legend.position = 'none') +
  labs(x = "",
       y = "Offspring Relative Ear Length")


# el <- cowplot::plot_grid(EL_midparent, NULL, parent_offspring_EL_plot, labels = c('E)', '', 'F)'), align = "h",
#                          axis = "bt", rel_widths = c(1, 0.05, 1.2), ncol = 3, nrow = 1,
#                          label_fontfamily = "Palatino", label_size = 12, label_x = 0.05, hjust = 0.8)




fig_S3 <- cowplot::plot_grid(parents_offspring_BW_plot, parent_offspring_TL_plot, parent_offspring_EL_plot,
                             ncol=1, nrow = 3, labels = c('A)', 'B)', 'C)'), label_fontfamily = "Palatino",
                             label_size = 12, label_x = 0, hjust = 0, align = "hv")

fig_S3 +
  geom_richtext(data=tibble(x=0.55, y=0.745), # Moms BW
            aes(x=x, y=y, label=glue("New York (*h^2* = {NY_moms_offspring_BW_h2} &#177; {NY_moms_offspring_BW_se})<br>Brazil (*h^2* = {BZ_moms_offspring_BW_h2} &#177; {BZ_moms_offspring_BW_se})*")),
            fill = NA, label.color = NA,
            size=2, family = "Palatino", label.padding = grid::unit(3, "mm"),
            inherit.aes = FALSE, hjust = 1) +
  geom_richtext(data=tibble(x=1, y=0.745), # Dads BW
                aes(x=x, y=y, label=glue("New York (*h^2* = {NY_dads_offspring_BW_h2 }&#177; {NY_dads_offspring_BW_se})<br>Brazil (*h^2* = {BZ_dads_offspring_BW_h2} &#177; {BZ_dads_offspring_BW_se})*")),
                fill = NA, label.color = NA,
                size=2, family = "Palatino", label.padding = grid::unit(3, "mm"),
                inherit.aes = FALSE, hjust = 1) +
  geom_richtext(data=tibble(x=0.54, y=0.41), # Moms TL
                aes(x=x, y=y, label=glue("New York (*h^2* = {NY_moms_offspring_RelTL_h2} &#177; {NY_moms_offspring_RelTL_se})<br>Brazil (*h^2* = {BZ_moms_offspring_RelTL_h2} &#177; {BZ_moms_offspring_RelTL_se})*")),
                fill = NA, label.color = NA,
                size=2, family = "Palatino", label.padding = grid::unit(3, "mm"),
                inherit.aes = FALSE, hjust = 1) +
  geom_richtext(data=tibble(x=1, y=0.41), # Dads TL
                aes(x=x, y=y, label=glue("New York (*h^2* = {NY_dads_offspring_RelTL_h2} &#177; {NY_dads_offspring_RelTL_se})<br>Brazil (*h^2* = {BZ_dads_offspring_RelTL_h2} &#177; {BZ_dads_offspring_RelTL_se})")),
                fill = NA, label.color = NA,
                size=2, family = "Palatino", label.padding = grid::unit(3, "mm"),
                inherit.aes = FALSE, hjust = 1) +
  geom_richtext(data=tibble(x=0.55, y=0.075), # Moms EL
                aes(x=x, y=y, label=glue("New York (*h^2* = {NY_moms_offspring_RelEL_h2} &#177; {NY_moms_offspring_RelEL_se})<br>Brazil (*h^2* = {BZ_moms_offspring_RelEL_h2} &#177; {BZ_moms_offspring_RelEL_se})*")),
                fill = NA, label.color = NA,
                size=2, family = "Palatino", label.padding = grid::unit(3, "mm"),
                inherit.aes = FALSE, hjust = 1) +
  geom_richtext(data=tibble(x=1, y=0.075), # Dads EL
                aes(x=x, y=y, label=glue("New York (*h^2* = {NY_dads_offspring_RelEL_h2} &#177; {NY_dads_offspring_RelEL_se})<br>Brazil (*h^2* = {BZ_dads_offspring_RelEL_h2} &#177; {BZ_dads_offspring_RelEL_se})*")),
                fill = NA, label.color = NA,
                size=2, family = "Palatino", label.padding = grid::unit(3, "mm"),
                inherit.aes = FALSE, hjust = 1)



#ggsave("results/figures/FigS3_h2.tiff", height = 8, width = 4, compression = "lzw")
ggsave("results/figures/FigS3_h2.pdf", height = 8, width = 4)
