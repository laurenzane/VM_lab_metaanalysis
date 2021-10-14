library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(lmerTest)
library(esc)

Lansade_2018_data <- read.csv("Lansade_2018_mares_foals_combo.csv")
#explanatory variables Cort.J.30, mare_or_foal, weaning.group
#random variable animal_id

Lansade_2018_lm <- lm(T.S.telo..95 ~ Cort.J.30 + mare_or_foal + weaning.group, data = Lansade_2018_data)

summary(Lansade_2018_lm)$r.squared
summary(Lansade_2018_lm)
#r.squared is 0.2693
#use multiple R-squared