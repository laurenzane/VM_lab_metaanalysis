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

Young_2017_1_omit_lmer <- lmer(TRFd25std ~ CORT + EnlargeExp + Order + Food + Sex
                               + (1|Mass2) + (1|Bird)  + (1|Sample) + (1|Date),
                               data = Young_2017_1_omit)
#error "grouping factors must have > 1 sampled level"

print(Young_2017_1_omit_lmer)
