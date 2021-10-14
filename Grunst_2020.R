library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(lmerTest)
library(esc)


Grunst_2020_raw_data <- read.csv('Grunst_2020_data.csv')
#omit because feather cort missing values
#RTL is relative telomere length initial, RTL8 is relative telomere length after treatment
#use this value
#or create delta TL column.  Ask David. 
#20201105 David says to use final TL 

Grunst_omit <- na.omit(Grunst_2020_raw_data)
Grunst_omit_lmer <- lmer(RTL8~ Treatment + Feather.CORT +Sex + Size.rank + Brood.size 
                         + (1|Date) + (1|Box), data = Grunst_omit)
print(Grunst_omit_lmer)

r.squaredGLMM(Grunst_omit_lmer)
#R^2 is 0.03413553
#n is 168

summary(Grunst_omit_lmer)


anova(Grunst_omit_lmer)

convert_r2z(0.03454124)
