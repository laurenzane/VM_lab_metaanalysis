library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(lmerTest)
library(esc)

Bauch_2016_data <- read.csv("Bauch_2016_raw.csv")

Bauch_2016_data_omit <- na.omit(Bauch_2016_data)

#bird_ID (individual bird identification number), 
#sex (male=1, female=2), 
#year (breeding year), 
#nest_ID (nest identification number per year), 
#age (age of bird), 
#n_fledglings (number of fledglings), 
#cort1 (baseline corticosterone measured early during incubation, ng/ml), 
#cort2 (baseline corticosterone measured mid incubation, ng/ml), 
#cort3 (baseline corticosterone measured at the end of incubation, ng/ml), 
#gel_ID (gel identification number), 
#TL (average telomere length, bp), 
#p10 (telomere length of the 10th percentile of the telomere length distribution, bp), 
#p20 (telomere length of the 20th percentile, bp), 
#p30 (telomere length of the 30th percentile, bp), 
#p40 (telomere length of the 40th percentile, bp), 
#p50 (telomere length of the 50th percentile, bp), 
#p60 (telomere length of the 60th percentile, bp), 
#p70 (telomere length of the 70th percentile, bp), 
#p80 (telomere length of the 80th percentile, bp), 
#p90 (telomere length of the 90th percentile, bp). 

#what is p10, p20, p30, p40, p50, p60, p70, p80, p90??? how to incorporate this
#independent variable: TL is an average TL, should I average the cort levels?
#explanatory variables: n_fledglings, cort (1-3????), age, sex
#random variables: nest_ID
Bauch_2016_data_omit$TL <- as.numeric(Bauch_2016_data_omit$TL)
Bauch_2016_lmer <- lmer (TL ~ cort3 + age + n_fledglings + sex
                       +(1|nest_ID), data = Bauch_2016_data_omit)

print(Bauch_2016_lmer)

summary(Bauch_2016_lmer)

r.squaredGLMM(Bauch_2016_lmer)

#R^2 is  0.09574449
#n is 27
#says not meaningful for factors. this was resolved 1/11/21
#literally i think the R^2 is zero.. also resolved 1/11/21

sessionInfo()
