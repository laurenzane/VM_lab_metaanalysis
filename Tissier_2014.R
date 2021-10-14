library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(lmerTest)
library(esc)

Tissier_2014 <- read.csv("Tissier_2014_data.csv")
Tissier_2014_omit <- na.omit(Tissier_2014)

#explanatory variable Treatment, Sex
#random variable Nest 

Tissier_2014_lmer <- lmer (Telo_length ~ Treatment + Sex 
                           + (1|Nest), data = Tissier_2014_omit)

r.squaredGLMM(Tissier_2014_lmer)

#r squared is 0.05963878
#n is 58