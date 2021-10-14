library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(lmerTest)
library(esc)

Hau_2015 <- read.csv("Hau_2015_excel_2.csv")
#multiple time points for cort and tl
#need to look at more closely because the dates aren't the same for tl and cort

#12292020
#explanatory variables StressCort_Dec09, treatment.type, Sex
#not sure why ACTH, Dex cort was measured, should I ask David or not worry about it since I'm not using 
#ring number ... what is that????
#random variables Nest_id

Hau_2015$Sex <- as.factor(Hau_2015$Sex)
Hau_2015$telomeres_Jan11 <- as.numeric(Hau_2015$telomeres_Jan11)
Hau_2015$StressCort_Dec09 <- as.numeric(Hau_2015$StressCort_Dec09)
Hau_2015_lmer <- lmer(telomeres_Jan11 ~ StressCort_Dec09 + treatment.type + Sex
                      + (1|Nest_id), data = Hau_2015)
r.squaredGLMM(Hau_2015_lmer)
#R2 is 0.1130105