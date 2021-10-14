library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(lmerTest)
library(esc)

Burraco_2019_TL <- read.csv("data_Burraco_et_al_tl.csv")
Burraco_2019_cort <- read.csv("data_Burraco_et_al_cort.csv")

Burraco_2019_TL_and_cort_full <- full_join(Burraco_2019_TL, Burraco_2019_cort, by = "Id")
Burraco_2019_TL_and_cort_subset <- inner_join(Burraco_2019_TL, Burraco_2019_cort, by = "Id")

#build lmer for Burraco_2019.  Independent variables: phenology, photoperiod, corticosterone
#random variables: clutch, Id
#catch up point same for all frogs

Burraco_2019_lmer <- lmer(rTL ~ Phenology.x + Photoperiod.x + Corticosterone + 
                            (1|Clutch.x),
     data=Burraco_2019_TL_and_cort_subset)


print(Burraco_2019_lmer)


r.squaredGLMM(Burraco_2019_lmer)

summary(Burraco_2019_lmer)

#R^2 is 0.006041308
#n is 94 

anova(Burraco_2019_lmer)

convert_r2z(0.006041308 )
