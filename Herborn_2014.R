library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(lmerTest)
library(esc)

Herborn_2014_data <- read.csv("Herborn_2014.csv")

Herborn_2014_omit <- na.omit(Herborn_2014_data)

#explanatory variables: D30.cort.stress, Treatment.label, Sex, Brood.size.D30
#random variable: NestID

Herborn_2014_lmer <- lmer(Telomere.D30 ~ D30.cort.stress + Treatment.label 
                          + Sex + (1|Brood.size.D30), data = Herborn_2014_omit)
r.squaredGLMM(Herborn_2014_lmer)

#R^2 is 0.1852
#n=104