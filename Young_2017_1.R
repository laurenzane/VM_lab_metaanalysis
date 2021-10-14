library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(lmerTest)
library(esc)

Young_2017_1 <- read.csv("BLKI Data_forDRYAD.csv")

Young_2017_1_omit <- na.omit(Young_2017_1)

#all same year, colony, foster status age
#order means hatching order (A, B, C), first chick is most "favored"
#use TRFd25std because this is the final telomere length 
#explanatory variable: CORT, EnlargeExp, Order, Food, Sex
#random variables: Mass2, Bird, HomeNest, Sample, Date

Young_2017_1_omit_lmer <- lmer(TRFd25std ~ CORT + EnlargeExp + Order + Food + Sex + Mass2
                             + (1|Nest)  + (1|Date),
                               data = Young_2017_1_omit)
#error "grouping factors must have > 1 sampled level"
#singular fit means overloaded model 
#run a bunch of models, compare using AIC

print(Young_2017_1_omit_lmer)
r.squaredGLMM(Young_2017_1_omit_lmer)

#r squared is 0.08261665
#n is 63
mynames <- paste("cm", as.character(1:24), sep = "")
myaicc <- aictab(list( cm1, cm2, cm3,  cm4,  cm5, cm6, cm7, cm8, cm9, cm10, 
                       cm11, cm12, cm13,  cm14,  cm15, cm16, cm17, cm18, cm19, cm20,
                       cm21, cm22, cm23, cm24), second.ord = TRUE, modnames = mynames)
myaicc

#cm is the first part of the name, as.character(1:24) number of models, my aicc
#difference between models is greater than 2, different models
#difference less than two, models are fundamentally the same
#non-biased model selection