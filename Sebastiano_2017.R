library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(lmerTest)
library(esc)

Sebastiano_2017_data <- read.csv("Sebastiano_2019_data.csv")
#independent variable: telomere
#explanatory variables: cort, groups.based.on.health.status
#random variables: sampling.period

Sebastiano_2017_lmer <- lmer (telomere ~ cort + groups.based.on.health.status
                              + (1|sampling.period),
                              data = Sebastiano_2017_data)
print(Sebastiano_2017_lmer)

r.squaredGLMM(Sebastiano_2017_lmer)

#R^2 is 0.1306194
#n is 62

summary(Sebastiano_2017_lmer)

anova(Sebastiano_2017_lmer)

convert_r2z(0.1285789)

#z-score 0.1292946