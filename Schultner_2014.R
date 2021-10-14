library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(lmerTest)
library(esc)

Schultner_2014_data <- read.csv("Schultner_2014.csv")
#waiting to ask about delta TL 
#dependent variable 
#independent variables stress.treatment, sex, wintering.time
#random variable animal.id 
#ask about stress treament cort versus oil placebo 20201112

Schultner_2014_lm <- lm(telomere.length.2012 ~ stress.treatment 
                            , data = Schultner_2014_data)
#random effects only work if you have multiple obs from one of the random effects 
#ex: muliple offspring, ex: animal id if multiple blood draws
#random takes into account the non randomness of the variables

summary(Schultner_2014_lm)
#rsquared is -0.08257
#n is 14


#20201118 "number of levels of each grouping factor must be < than number of observations"
#change to int or num???

Schultner_2014_data$animal.id <- as.factor(Schultner_2014_data$animal.id)

Schultner_2014_data$wintering.time <- as.numeric(Schultner_2014_data$wintering.time)
#ALERT CANNOT USE BECAUSE CORT IS A TREATMENT NOT A NUMERIC