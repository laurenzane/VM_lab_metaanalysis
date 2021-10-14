library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(lmerTest)
library(esc)

Casagrande_2020_full_data <- read.csv("data_Casagrande_et_al._2020_JEB.csv")
Casagrande_2020_omit <- na.omit(Casagrande_2020_full_data)

#Casagrande_2020 did not have telomere lengths for all individuals vbecause the TRF did not work properly
#nestbox number is high because 150 nest boxes were checked to monitor for hatchling, could be a random effect
#independent variables: treatment, cort
#random variables: nest

Casagrande_2020_omit_lmer <- lmer(telomere ~ cort + treatment +
                                    (1|nest),
                                  data=Casagrande_2020_omit)
print(Casagrande_2020_omit_lmer)

r.squaredGLMM(Casagrande_2020_omit_lmer)

#R^2 is 0.2514671
#N is 20

summary(Casagrande_2020_omit_lmer)

anova(Casagrande_2020_omit_lmer)

convert_r2z(0.2514671)
