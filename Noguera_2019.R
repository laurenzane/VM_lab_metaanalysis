library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(lmerTest)
library(esc)

#202021118 this data is unusable because of the log cort

#I think that TS is telomere...size?  unsure but in omitted values there is a note on RBC or plasma 
#so it is probably that?
#ok...T/S value is TS.  It is the quantity of telomere product to a single copy gene
#can I unlog the CORT values?  0r should I just use logCORT?
#20201105 David said try to find what log was used 

Noguera_2019_data <- read.csv("Noguera_2019.csv")
Noguera_2019_omit <- na.omit(Noguera_2019_data)

#dependent variable TS
#independent variables treatment, logCORT, sex, 
#random year, weight (W), incutime, incu
#what is incu? 
#incu is the incubator.  There were 5 incubators to avoid incubator effects

Noguera_2019_omit_lmer <- lmer(TS ~ Treat + logCORT + sex +
                                 (1|year)+ (1|W) + (1|incutime)+ (1|incu), data = Noguera_2019_omit)
print(Noguera_2019_omit_lmer)

r.squaredGLMM(Noguera_2019_omit_lmer)

summary(Noguera_2019_omit_lmer)

#R^2 is 0.1435473
#n is 68

#you cannot use logCORT sorry!  
anova(Noguera_2019_omit_lmer)

convert_r2z(0.1435473)


