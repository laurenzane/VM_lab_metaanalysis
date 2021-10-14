setwd("~/Desktop/VM_lab_honors_thesis/VM_lab_honors_thesis")

library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(lmerTest)
library(esc)

Lemaitre_2021_data <- read.csv("Lemaitre_2021_data.csv")
#explanatory variables: FGM_2017 + Population + Sex 
#random variable Cohort + CaptureGap

Lemaitre_2021_lmer <- lmer(QC_RTL_2017 ~ FGM_2017 + Population + Sex 
                           + (1|Cohort), data = Lemaitre_2021_data)


summary(Lemaitre_2021_lmer)
r.squaredGLMM(Lemaitre_2021_lmer)
#R squared  0.09928592
#n = 43
