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

##############################################################
# Import data
##############################################################

PostDissectionMetaData <- read_csv(here("data/raw/post_dissection_metadata_RAW_2021-02-18.csv")) %>%
  select(-BMI_kg_m2, -BAT_mass_g, -X24) %>%
  filter(Population == "BRAZIL" | Population == "NEW_YORK") %>% # only keep parental populations (remove F1 hybrids)
  filter(Generation == "N11" | Generation == "N12") %>%
  mutate(Sex = fct_recode(Sex, "Female" = "F", "Male" = "M")) %>% # spells out males and females
  mutate(Sex = fct_relevel(Sex, "Male", "Female")) %>% # puts males before females
  mutate(Environment = fct_recode(Environment, "Cold" = "COLD", "Warm" = "RT")) %>%
  mutate(Line = fct_recode(Line, "MANA" = "193x255", "SARA" = "19x13",
                           "MANB" = "222x254", "SARB" = "82x81")) %>% # gives each line the "published"/JAX name
  mutate(Population = fct_recode(Population, "Brazil" = "BRAZIL", "New York" = "NEW_YORK")) %>% # modifies names of populations
  mutate(Population = fct_relevel(Population, "New York", "Brazil")) %>%
  mutate(Line = fct_relevel(Line, "MANA", "MANB", "SARA", "SARB")) %>% # puts Brazil lines before New York lines
  mutate(Environment = fct_relevel(Environment, "Warm", "Cold" )) %>% # puts Warm before Cold
  mutate(PopEnv = paste(Population, Environment, sep = "_")) %>%
  mutate(PopEnv = fct_recode(PopEnv, "Brazil - Warm" = "Brazil_Warm", "Brazil - Cold" = "Brazil_Cold",
                             "New York - Warm" = "NewYork_Warm", "New York - Cold" = "NewYork_Cold")) %>%
  mutate(PopEnv = fct_relevel(PopEnv, "New York - Warm", "New York - Cold", "Brazil - Warm", "Brazil - Cold")) %>%
  mutate(PopEnvSex = paste(Population, Environment, Sex, sep = "_")) %>%
  mutate(PopEnvSex = fct_recode(PopEnvSex,
                                "Brazil - Warm - Male" = "Brazil_Warm_Male", "Brazil - Cold - Male" = "Brazil_Cold_Male",
                                "Brazil - Warm - Female" = "Brazil_Warm_Female", "Brazil - Cold - Female" = "Brazil_Cold_Female",
                                "New York - Warm - Male" = "NewYork_Warm_Male", "New York - Cold - Male" = "NewYork_Cold_Male",
                                "New York - Warm - Female" = "NewYork_Warm_Female", "New York - Cold - Female" = "NewYork_Cold_Female")) %>%
  mutate(PopEnvSex = fct_relevel(PopEnvSex, "Brazil - Warm - Male", "Brazil - Warm - Female", "Brazil - Cold - Male", "Brazil - Cold - Female",
                                 "New York - Warm - Male", "New York - Warm - Female", "New York - Cold - Male", "New York - Cold - Female"))



# Get residuals of tail length and body weight
lmTLBW <- lm(FinalTailLength_mm~BodyWeight_g, data = PostDissectionMetaData)
PostDissectionMetaData$Resids_TLBW <- resid(lmTLBW) # saves residuals to Nachman_TailData dataset (the dataset used to create lm and residuals)


# Sex-specific datasets
MaleData <- PostDissectionMetaData %>%
  filter(Sex == "Male")

FemaleData <- PostDissectionMetaData %>%
  filter(Sex == "Female")


##############################################################
# Do some data exploration and model testing
##############################################################

# From exploratory analyses (see 'Modeling_RXN_2021-02-19.Rmd', we don't have to transform body weight data


# Some model testing - first, non-transformed data
# mod.full.BW <- lme(BodyWeight_g ~ Population * Environment * Sex,
#                    random = ~1|Line,
#                    data = PostDissectionMetaData)
# summary(mod.full.BW)
# hist(resid(mod.full.BW), breaks = 25)
# qqnorm(resid(mod.full.BW))
# qqline(resid(mod.full.BW))
# shapiro.test(resid(mod.full.BW)) # normally distributed
# 
# mod.full.BW.2 <- lme(BodyWeight_g ~ Population + Environment + Sex,
#                      random = ~1|Line,
#                      data = PostDissectionMetaData)
# summary(mod.full.BW.2)
# hist(resid(mod.full.BW.2), breaks = 25)
# qqnorm(resid(mod.full.BW.2))
# qqline(resid(mod.full.BW.2))
# shapiro.test(resid(mod.full.BW.2)) #  normally distributed
# 
# AIC(mod.full.BW, mod.full.BW.2)
# anova(mod.full.BW, mod.full.BW.2) # these two are essentially the same

# # Log-transformed data
# mod.full.logBW <- lme(LogBW ~ Population * Environment * Sex,
#                    random = ~1|Line,
#                    data = PostDissectionMetaData)
# summary(mod.full.logBW)
# hist(resid(mod.full.logBW), breaks = 25)
# qqnorm(resid(mod.full.logBW))
# qqline(resid(mod.full.logBW))
# shapiro.test(resid(mod.full.logBW)) #  normally distributed
# 
# mod.full.logBW.2 <- lme(LogBW ~ Population + Environment + Sex,
#                       random = ~1|Line,
#                       data = PostDissectionMetaData)
# summary(mod.full.logBW.2)
# hist(resid(mod.full.logBW.2), breaks = 25)
# qqnorm(resid(mod.full.logBW.2))
# qqline(resid(mod.full.logBW.2))
# shapiro.test(resid(mod.full.logBW.2)) # normally distributed
# 
# AIC(mod.full.logBW, mod.full.logBW.2)
# anova(mod.full.logBW, mod.full.logBW.2) # mod.full.logBW2 is a much better model

# ### Overall, don't need to transform data, so will use raw body weight data ###
# mod.BW.1 <- lme(BodyWeight_g ~ Population * Environment * Sex,
#                  method = "ML",
#                  random = ~1|Line,
#                  data = PostDissectionMetaData)
# 
# mod.BW.1b <- lme(BodyWeight_g ~ Population + Environment + Sex,
#                  method = "ML",
#                  random = ~1|Line,
#                  data = PostDissectionMetaData)
# 
# anova(mod.BW.1, mod.BW.1b)
# AIC(mod.BW.1, mod.BW.1b)
# 
# # The two models are essentially the same (though the NON-3-way interaction is better)
# 
# 
# lmm.full.BW <- lmer(BodyWeight_g ~ Population * Environment * Sex + (1|Line),
#                     data = PostDissectionMetaData)
# summary(lmm.full.BW)
# III.BW <- car::Anova(lmm.full.BW, type=3)
# III.BW
# 
# summary(fm1 <- aov(BodyWeight_g~PopEnvSex , data = PostDissectionMetaData)) # roughly gives same answer as full model (which is great!)
# tukeydata<-TukeyHSD(fm1, "PopEnvSex", ordered = TRUE)
# TukeyHSD(fm1, "PopEnvSex", ordered = TRUE) # tells you which comparisons are different
# plot(TukeyHSD(fm1, "PopEnv"))


##############################################################
# Plot rxn norm plots (body weight & tail length)
##############################################################

BWrxnM <-
  ggplot(data=MaleData, aes(x=Environment, y=BodyWeight_g)) +
  geom_boxplot(aes(fill = Population), position = position_dodge(width=0.25), alpha=0.8, size = 0.5, outlier.shape = NA, notch = FALSE) +
  stat_summary(
    fun = median,
    geom = 'line',
    aes(group = Population, color = Population),
    position = position_dodge(width = 0.25) , size=1.5, alpha = 1.2, linetype = "dashed") +
  geom_point(aes(fill = Population), position = position_jitterdodge(jitter.width = 0.15, dodge.width=0.25), alpha=5, size=3, shape=21, color = "white") +
  #annotate("text", x = 0.75, y=25, label="Males", family = "Palatino") +
  scale_color_manual(values=rep(c("dodgerblue4", "goldenrod1"))) +
  scale_fill_manual(values=rep(c("dodgerblue4", "goldenrod1"))) +
  #scale_x_discrete("Environment", expand=c(0.075,0.5)) +
  #guides(color=guide_legend(override.aes=list(fill=NA)),
        # linetype=guide_legend(override.aes = list(fill=NA))) +# removes gray shading from legend
  scale_y_continuous(breaks = seq(from=8, to=24, by=4), labels = seq(from=8, to=24, by=4), limits = c(8,25)) +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10), size = 12, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 10, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 10, color = "black", family = "Palatino"),
        legend.position = c(0.045, 0.93),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.5, "cm"),
        legend.text = element_text(size=8, family = "Palatino"),
        legend.title = element_blank(),
        plot.tag = element_text(family = "Palatino", size = 10, face = "italic"),
        plot.tag.position = c(0.2,1.015),
        plot.title = element_text(family = "Palatino"),
        plot.margin = unit(c(0.5, -0.5, 0.1, 0), "cm")) +
  labs(x = "",
       y = "Body Mass (g)",
       tag = "Males")

BWrxnF <-
  ggplot(data=FemaleData, aes(x=Environment, y=BodyWeight_g)) +
  geom_boxplot(aes(fill = Population), position = position_dodge(width=0.25), alpha=0.8, size = 0.5, outlier.shape = NA, notch = FALSE) +
  stat_summary(
    fun = median,
    geom = 'line',
    aes(group = Population, color = Population),
    position = position_dodge(width = 0.25) , size=1.5, alpha = 1.2, linetype = "dashed") +
  geom_point(aes(fill = Population), position = position_jitterdodge(jitter.width = 0.15, dodge.width=0.25), alpha=5, size=3, shape=21, color = "white") +
  annotate("text", x = 2.4, y=25, label="****sex\n*pop", family = "Palatino", fontface = 3, size = 3, hjust = 1) +
  scale_color_manual(values=rep(c("dodgerblue4", "goldenrod1"))) +
  scale_fill_manual(values=rep(c("dodgerblue4", "goldenrod1"))) +
  #scale_x_discrete("Environment", expand=c(0.075,0.5)) +
  scale_y_continuous(breaks = seq(from=8, to=24, by=4), labels = seq(from=8, to=24, by=4), limits = c(8,25)) +
  theme_half_open(20) +
  panel_border() + # puts border around facets
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10), size = 12, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 10, color = "black", family = "Palatino"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "none",
        plot.tag = element_text(family = "Palatino", size = 10, face = "italic"),
        plot.tag.position = c(0.14,1.015),
        plot.title = element_text(family = "Palatino"),
        plot.margin = unit(c(0.5, 0.5, 0.1, -0.5), "cm")) + # top, right, bottom, left (0.5, 0.5, 0.1, -0.5)
  labs(x = "",
       y = "",
       tag ="Females")

TLrxn <-
  ggplot(data=PostDissectionMetaData, aes(x=Environment, y=Resids_TLBW)) +
  geom_boxplot(aes(fill = Population), position = position_dodge(width=0.25), alpha=0.8, size = 0.5, outlier.shape = NA, notch = FALSE) +
  stat_summary(
    fun = median,
    geom = 'line',
    aes(group = Population, color = Population),
    position = position_dodge(width = 0.25) , size=1.5, alpha = 1.2, linetype = "dashed") +
  geom_point(aes(fill = Population), position = position_jitterdodge(jitter.width = 0.15, dodge.width=0.25), alpha=5, size=3, shape=21, color = "white") +
  annotate("text", x = 2.4, y=9.5, label="****pop\n*env\n**pop x env", family = "Palatino", fontface = 3, size = 3, hjust = 1) +
  scale_color_manual(values=rep(c("dodgerblue4", "goldenrod1"))) +
  scale_fill_manual(values=rep(c("dodgerblue4", "goldenrod1"))) +
  #scale_x_discrete("Environment", expand=c(0.075,0.5)) +
  scale_y_continuous(breaks = seq(from=-10, to=10, by=5), labels = seq(from=-10, to=10, by=5), limits = c(-10,10)) +
  theme_bw() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10), size = 12, face = "bold", family = "Palatino"),
        axis.text.x = element_text(size = 10, color = "black", family = "Palatino"),
        axis.text.y = element_text(size = 10, color = "black", family = "Palatino"),
        strip.background =element_rect(fill="white"),
        strip.text = element_text(colour = 'black', face = "bold", family = "Palatino", size = 10, lineheight = 0.5),
        legend.position = "none",
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.5, "cm"),
        legend.text = element_text(size=7, family = "Palatino"),
        legend.title = element_blank(),
        # plot.tag = element_text(family = "Palatino", size = 10, face = "italic"),
        # plot.tag.position = c(0.13,1.015),
        # plot.title = element_text(family = "Palatino"),
        plot.margin = unit(c(0.5, 0, 0.1, 0.5), "cm")) +
  labs(x = "",
       y = "Tail Length Residuals")#,
       #tag = "Males")

cowplot::plot_grid(BWrxnM, BWrxnF, TLrxn, ncol = 3, nrow = 1, labels = c('A', '', 'B'), label_fontfamily = "Palatino")

ggsave("figures/RXNs.tiff", height = 4, width = 9, compression = "lzw")
ggsave("figures/RXNs.pdf", height = 4, width = 9)

dev.off()

