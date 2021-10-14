library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(lmerTest)
library(esc)

Watson_2016_data <- read.csv("Watson_2016_data.csv")
Watson_2016_omit <- na.omit(Watson_2016_data)

#explanatory variables for T.S.Pfaffl.corrected are CORT.ng.ml, Sex, No..Days.Handled
#no random variables  
Watson_2016_omit$No..Days.Handled <- as.factor(Watson_2016_omit$No..Days.Handled)
Watson_2016_lm <- lm(T.S.Pfaffl.corrected ~ CORT.ng.ml
                     + T.S.Pfaffl.corrected
                     + Sex
                     + No..Days.Handled, data = Watson_2016_omit)

summary(Watson_2016_lm)
#R^2 is 0.1977
#n is 25
