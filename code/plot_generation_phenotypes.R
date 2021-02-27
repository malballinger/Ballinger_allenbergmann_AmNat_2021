#!/usr/bin/env Rscript --vanilla

##############################################################
# Author: Mallory A. Ballinger
# Script first created: 24-Feb-2021
# Script last updated:  25-Feb-2021


# This script plots body weight and tail lengths of New York and Brazil mice across generations,
# and generates Figure 2 in Ballinger_et_al_2021_AmNat


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

##############################################################
# Import data
##############################################################

GenerationMetaData <- read_csv(here("data/processed/AllBZNYmice_wild&colony.csv")) %>%
  rename("Population" = "POPULATION", "Generation" = "GENERATION", "BodyLength_mm" = "BODY_LENGTH_mm",
         "TailLength_mm" = "TAIL_LENGTH_mm", "EarLength_mm" = "EAR_LENGTH_mm",
         "BodyMass_g" = "MASS_g", "Age_days" = "AGE_DAYS") %>%
  mutate(RelativeTail = TailLength_mm / BodyLength_mm) %>%
  mutate(RelativeEar = EarLength_mm / BodyLength_mm) %>%
  mutate(SEX = fct_recode(SEX, "Female" = "F", "Male" = "M")) %>% # spells out males and females
  mutate(Population = fct_recode(Population, "Brazil" = "BRAZIL", "New York" = "NEW_YORK")) %>%
  mutate(SEX = fct_relevel(SEX, "Male", "Female")) %>% # puts males before females
  mutate(Population = fct_relevel(Population, "New York", "Brazil")) %>%
  filter(Generation == "N0" | Generation == "N1" | Generation == "N2") # only select generations N0-N3 for downstream analyses

# Sample sizes of full dataset (body mass)
FullSampleSize <- GenerationMetaData %>% summarize(N=n_distinct(COLLECTOR_ID_BIOSAMPLE_ID)) %>% pull(N)

##############################################################
# Do some data exploration and model testing - Body Mass
##############################################################

# From exploratory analyses (see '2021-02-08_RidgelinePlots.Rmd'), we don't have to transform any data
# both models (log-transformed body weight data and non-log tranformed data) give roughly the same results,
#just slightly diff p-values (sign. Generation:Pop & Pop:Sex interactions)

# So I'll just stick with raw body mass since I'm not transforming body mass in any other plot/analysis


#### Model building
#hist(GenerationMetaData$BodyMass_g, breaks = 25)

## Full model
mod.full.BW <- lm(BodyMass_g ~ Generation * Population * SEX,
                  data = GenerationMetaData)
# plot(mod.full.BW)
# hist(resid(mod.full.BW), breaks = 25)
# qqnorm(resid(mod.full.BW))
# qqline(resid(mod.full.BW))
# shapiro.test(resid(mod.full.BW))


#summary(mod.full.BW)

#Anova(mod.full.BW, type="III")



## Sex-specific body mass datasets
Maledata <- GenerationMetaData %>%
  filter(SEX == "Male")

Femaledata <- GenerationMetaData %>%
  filter(SEX == "Female")


##############################################################
# Do some data exploration and model testing - Tail Length
##############################################################

#### Model building
# hist(GenerationMetaData$TailLength_mm, breaks = 25)
# 
# ## Tail length and body weight correlation
# lmTLBW <- lm(TailLength_mm~BodyMass_g, data = GenerationMetaData)
# hist(resid(lmTLBW), breaks = 25)
# qqnorm(resid(lmTLBW))
# qqline(resid(lmTLBW))
# shapiro.test(resid(lmTLBW))

# there are some clear outliers
# no tail should be larger than 140mm
# an no tail should be smaller than 50 mm

GenerationMetaData2 <- GenerationMetaData %>%
  filter(TailLength_mm > 50, TailLength_mm < 140)
# hist(GenerationMetaData2$TailLength_mm, breaks = 25)

## Run the model again
lmTLBW.2 <- lm(TailLength_mm~BodyMass_g, data = GenerationMetaData2,
               na.action = na.exclude)
# hist(resid(lmTLBW.2), breaks = 25)
# qqnorm(resid(lmTLBW.2))
# qqline(resid(lmTLBW.2))
# shapiro.test(resid(lmTLBW.2)) ### normally distributed!!!


## Full model
mod.full.TL <- lm(TailLength_mm~BodyMass_g + Generation * Population * SEX,
                  data = GenerationMetaData2)
# plot(mod.full.TL)
# hist(resid(mod.full.TL), breaks = 25)
# qqnorm(resid(mod.full.TL))
# qqline(resid(mod.full.TL))
# shapiro.test(resid(mod.full.TL)) # beautiful!

# save residuals for plotting
#GenerationMetaData2$Resids_TLBW_mod <- resid(mod.full.TL)
GenerationMetaData2$Resids_TLBW <- resid(lmTLBW.2)


#summary(mod.full.TL)

#Anova(mod.full.TL, type="III")



## Sex-specific body mass datasets
Maledata2 <- GenerationMetaData2 %>%
  filter(SEX == "Male")

Femaledata2 <- GenerationMetaData2 %>%
  filter(SEX == "Female")

##############################################################
# Plot sex-specific body mass ridgeplot across generations
##############################################################

Mmassridge <- 
  ggplot(data = Maledata, aes(y=Generation, x=BodyMass_g)) +
  geom_density_ridges(aes(point_color = Population, point_fill = Population, fill = Population),
                      jittered_points = TRUE, position = "raincloud",
                      alpha = 0.8, point_size = 2, point_alpha = 0.9, scale = 0.3, point_shape=21, point_color = "white", color = NA) +
  geom_boxplot(aes(x = BodyMass_g, y = Generation, color = Population),
               position = position_dodge(width = 0.01),  outlier.shape = NA, alpha = 0.5, width = 0.17) +
  scale_x_continuous(breaks = seq(from=5, to=45, by=10), labels = seq(from=5, to=45, by=10), limits = c(5,45)) +
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
        axis.title.y = element_text(margin = margin(r = 10), size = 11, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 9, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 9, color = "black", family = "Palatino"),
        legend.position = c(0.045, 0.93),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.5, "cm"),
        legend.text = element_text(size=8, family = "Palatino"),
        legend.title = element_blank(),
        plot.tag = element_text(family = "Palatino", size = 10, face = "italic"),
        plot.tag.position = c(0.15,1.025),
        plot.title = element_text(family = "Palatino", face = "plain"),
        plot.margin = unit(c(0.5, -0.5, 0.1, 0.5), "cm")) +
  labs(x = "Body Mass (g)",
       y = "",
       tag = "Males")

Fmassridge <- 
  ggplot(data = Femaledata, aes(y=Generation, x=BodyMass_g)) +
  geom_density_ridges(aes(point_color = Population, point_fill = Population, fill = Population),
                      jittered_points = TRUE, position = "raincloud",
                      alpha = 0.8, point_size = 2, point_alpha = 0.9, scale = 0.3, point_shape=21, point_color = "white", color = NA) +
  geom_boxplot(aes(x = BodyMass_g, y = Generation, color = Population),
               position = position_dodge(width = 0.01),  outlier.shape = NA, alpha = 0.5, width = 0.17) +
  annotate("text", x = 44, y=3.4, label="**gen x pop\n*pop x sex", family = "Palatino", fontface = 3, size = 3, hjust = 1) +
  scale_x_continuous(breaks = seq(from=5, to=45, by=10), labels = seq(from=5, to=45, by=10), limits = c(5,45)) +
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
        axis.text.x = element_text(size = 9, color = "black", family = "Palatino"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = 'none',
        plot.tag = element_text(family = "Palatino", size = 10, face = "italic"),
        plot.tag.position = c(0.1,1.025),
        plot.margin = unit(c(0.5, 0.5, 0.1, -0.5), "cm")) + # top, right, bottom, left (0.5, 0.5, 0.1, -0.5)
  labs(x = "",
       y = "",
       tag = "Females")


##############################################################
# Plot sex-specific tail length ridgeplot across generaitons
##############################################################

Mtailridge <- 
  ggplot(data = Maledata2, aes(y=Generation, x=Resids_TLBW)) +
  geom_density_ridges(aes(point_color = Population, point_fill = Population, fill = Population),
                      jittered_points = TRUE, position = "raincloud",
                      alpha = 0.8, point_size = 2, point_alpha = 0.9, scale = 0.3, point_shape=21, point_color = "white", color = NA) +
  geom_boxplot(aes(x = Resids_TLBW, y = Generation, color = Population),
               position = position_dodge(width = 0.01),  outlier.shape = NA, alpha = 0.5, width = 0.17) +
  scale_x_continuous(breaks = seq(from=-20, to=20, by=10), labels = seq(from=-20, to=20, by=10), limits = c(-20,20)) +
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
        axis.title.x = element_text(margin = margin(t = 10), size = 11, face = "bold", family = "Palatino", hjust = 1.15),
        axis.title.y = element_text(margin = margin(r = 10), size = 11, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 9, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 9, color = "black", family = "Palatino"),
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
        plot.title = element_text(family = "Palatino", face = "plain"),
        plot.margin = unit(c(0.5, -0.5, 0.1, 0.5), "cm")) +
    labs(x = "Tail Length Residuals",
         y = "Generation",
         tag = "Males")

Ftailridge <- 
  ggplot(data = Femaledata2, aes(y=Generation, x=Resids_TLBW)) +
  geom_density_ridges(aes(point_color = Population, point_fill = Population, fill = Population),
                      jittered_points = TRUE, position = "raincloud",
                      alpha = 0.8, point_size = 2, point_alpha = 0.9, scale = 0.3, point_shape=21, point_color = "white", color = NA) +
  geom_boxplot(aes(x = Resids_TLBW, y = Generation, color = Population),
               position = position_dodge(width = 0.01),  outlier.shape = NA, alpha = 0.5, width = 0.17, fill = "white") +
  annotate("text", x = 19, y=3, label="**gen\n**gen x sex", family = "Palatino", fontface = 3, size = 3, hjust = 0) +
  scale_x_continuous(breaks = seq(from=-20, to=20, by=10), labels = seq(from=-20, to=20, by=10), limits = c(-20,20)) +
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
        axis.title.x = element_text(margin = margin(t = 10), size = 11, face = "bold", family = "Palatino", hjust = 0.6),
        axis.title.y = element_text(margin = margin(r = 10), size = 11, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 9, color = "black", family = "Palatino"),
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
        plot.tag = element_text(family = "Palatino", size = 10, face = "italic"),
        plot.tag.position = c(0.1,1.025),
        plot.title = element_text(family = "Palatino", face = "plain"),
        plot.margin = unit(c(0.5, 0.5, 0.1, -0.5), "cm")) + # top, right, bottom, left (0.5, 0.5, 0.1, -0.5)
  labs(x = "",
       y = "",
       tag = "Females")



cowplot::plot_grid(Mmassridge, Fmassridge, Mtailridge, Ftailridge, labels = c('A', '', 'B', ''), ncol=2, nrow=2, label_fontfamily = "Palatino")

ggsave("figures/generation_phenotypes.tiff", height = 7, width = 8, compression = "lzw")
ggsave("figures/generation_phenotypes.pdf", height = 7, width = 8)

dev.off()