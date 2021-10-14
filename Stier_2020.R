library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(lmerTest)
library(esc)

#model for TBC tl at StED13 
#merge two spreadsheets 
#double check lmer

20201118 #lmer is ok, stED13 is a time point
#embryos exposed to different incubation temp, this is designated in group.x (group.y), same variable
#U = unstable, H = high temp, M = med temp, L = low temp 
#only has embryo data, wonder if this is ok, ask David 

Stier_2019_data_1 <- read.csv("Data_Stier_JQ_stED13_2019.csv")
Stier_2019_data_2 <- read.csv("Data_Stier_JQ_stED13_TL_2019.csv")

Stier_2019_full <-  full_join(Stier_2019_data_1, Stier_2019_data_2, by = "ID")

Stier_2019_lm <- lm (RBC_TL ~ plasma.CORT._stED13 + Group.x , data = Stier_2019_full)

summary(Stier_2019_lm)


#R^2 is 0.0596
#n is 60

summary(Stier_2019_lmer)

anova(Stier_2019_lmer)

convert_r2z(0.04484218)