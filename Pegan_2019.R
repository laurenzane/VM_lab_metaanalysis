library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(lmerTest)
library(esc)

Pegan_2019_data <- read.csv("Pegan_2019_raw.csv")

Pegan_2019_data_omit <- na.omit(Pegan_2019_data)

#study measures baseline cort with or without cort treatment, then TRF at 12 days
#independent variable: TRF
#explanatory variables: treatment, sex, Stress_mean
#random variable: d12_mass

Pegan_2019_lmer <- lmer(TRF ~ treatment + Stress_mean + sex
                        + (1|d12_mass), data = Pegan_2019_data_omit)
print(Pegan_2019_lmer)

r.squaredGLMM(Pegan_2019_lmer)
#R^2 is 0.01347339
#n is 48

summary(Pegan_2019_lmer)

anova(Pegan_2019_lmer)

convert_r2z( 0.01347339)

#zscore = 0.01347421