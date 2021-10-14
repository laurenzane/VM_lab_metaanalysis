library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(lmerTest)
library(esc)

Injaian_data <- read.csv("Injaian_data.csv")
#this also has changes in baseline cort to stress cort and changes in telomere length
Injaian_data_omit <- na.omit(Injaian_data)

Injaian_data_omit_lmer <- lmer(Day14TRF ~ StrInducedCort + RearNoise 
                             + NatalNoise + RearAmp + NatalAmplitude
                             + (1|RearNestID)+ (1|NatalNestID), data = Injaian_data_omit)

r.squaredGLMM(Injaian_data_omit_lmer)
summary(Injaian_data_omit_lm)$r.squared
#R^2 is  0.3235411
#n is 17
#error "number of levels of each grouping factor must be < number of observations"
#resolved 12/2