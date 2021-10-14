library(dplyr)
library(metafor)
library(Matrix)
help("metafor")

#20201005 
#started calculating effect size of studies that had raw data publically available as a csv or xlsx file
#tableone package is useful to summarize categorical or continuous variables 
#continuous variables can be summarized in normal ways ie mean/std dev or with median/quartile




#load data for Gil_2019
gil_2019 <- read.csv("gil_2019_raw_data.csv")

#what is the treatment?  brood size, designated as 1 or 2

#define covariates 
vars <- c("chick_code","brood_order","expnest_ID","originalnest_ID","expbrood_code","originalbrood_code","date1","age1","mass1",
          "date2","age2","mass2","alive2","date3","age3","alive3","time3","mass3","wing3","tarsus3","date4","alive4",
          "tel_length","tel_plate","orig_bs","exp_bs","sex","gsh","mda","cort","tas")

#construct a table
gil_2019_tab_unmatched <- CreateTableOne(vars = vars, strata = "brood_size", data = gil_2019, test = FALSE)

#what is stratafied????  I think that it is a yes or no treatment

#show table with SMD
print(gil_2019_tab_unmatched, smd = TRUE)

#YAY R code does not have issues, seems to makes sense BUT
#this package (tableone) would be really great is there was a binary treatment for example yes/no cort treatment
#alas there are separate metrics of cort/telomere length so I think I will need to use a different package to
#calculate the smd from the correlation between the two variables, but I'm not sure how to incorporate
#the different co-variates I will need to incorporate.  may need to designate variables as categorical.

library(dplyr)
as.factor(gil_2019$brood_order)

#cort does not have a value for each entry, but telomere length does, so I will have to join all values
#in cort to values in telomere length before can compute mean and sd

gil_2019_omit <- na.omit(gil_2019)
as.factor(gil_2019_omit$brood_order)

gil_2019_tel_length_avg <- gil_2019_omit %>% 
  group_by(brood_order)%>%
  summarise(avg=mean(tel_length))%>%
  arrange(avg)

gil_2019_cort_avg <- gil_2019_omit %>%
  group_by(brood_order)%>%
  summarise(avg=mean(cort))%>%
  arrange(avg)

gil_2019_tel_length_sd <- gil_2019_omit %>% 
  group_by(brood_order)%>%
  summarise(sd=sd(tel_length))%>%
  arrange(sd)

gil_2019_cort_sd <- gil_2019_omit %>%
  group_by(brood_order)%>%
  summarise(sd=sd(cort))%>%
  arrange(sd)

#now you are able to calculate smd

install.packages("MBESS")
library(MBESS)

gil_2019_omit %>% 
  group_by(brood_order=="1")%>%
  smd(Group.1 = cort, Group.2 = tel_length, Mean.1 = 13.01, Mean.2 = 1.3, s.1 = 16.67, s.2 = 0.209, s = NULL, n.1 = NULL, n.2 = NULL,
      Unbiased=FALSE)
#now I think that Cohen's f2 is a better way to calculate the effect size but I want to be able to visualize
#this decision and create a scatter plot that includes separately colored brood orders

gil_2019_omit_brood_order_1 <- filter(gil_2019_omit, brood_order == "1")

gil_2019_omit_brood_order_2 <- filter(gil_2019_omit, brood_order == "2")

library(ggplot2)
as.factor(gil_2019_omit$brood_order)

gil_2019_omit_scatterplot <- ggplot(gil_2019_omit, aes(cort,tel_length)) + 
  geom_point(aes(color=brood_order))+
  xlab('cort') +
  ylab('tel_length')
print(gil_2019_omit_scatterplot)

#ok now it is time to start calculating the effect size, of the difference in correlation?
glm()

#stuck here

#10132020 met with David last week to talk about stats
#to create new variable and add it to the data set use dplyr::mutate

library(dplyr)
gil_2019_omit_new_variable <- gil_2019_omit %>% 
  mutate(brood_size = orig_bs-exp_bs)
summary(gil_2019_omit_new_variable)
as.factor <- (gil_2019_omit_new_variable$sex)

#glmer using TL as the dependent variable, sex/cort as independent variables, brood_size (org_bs minus exp_bs)
#mass at date 3, originalnest_ID, experimental_ID, as random factor.scope(originalnest_ID > exp_ID)
#lme4 package 

library(lme4)


gil_2019_omit_lmer <-lmer(tel_length ~ cort + sex + brood_size+
                           
                            (1|originalnest_ID)+(1|expnest_ID), 
                          data=gil_2019_omit_new_variable)
print(gil_2019_omit_lmer)

r.squaredGLMM(gil_2019_omit_lmer)

#R^2 is 0.00884815
#n is 239
#downside to lmer is no p values

gil_2019_omit_glm <- glm(tel_length ~ cort + sex, 
                         family=gaussian, 
                         data = gil_2019_omit_new_variable)
print(gil_2019_omit_glm)

#glm 2, only cort as independent variable

gil_2019_omit_glm2 <- glm(tel_length ~ cort + brood_size, 
                         family=gaussian, 
                         data = gil_2019_omit_new_variable)
print(gil_2019_omit_glm2)

#AIC is low (about -60), intercepts barely above or below zero, basically seems like no correlation
#trying glmer again, but it gave me shit last time for using gaussian, literally wouldn't run
#used poisson, but poisson didn't return an AIC and I felt like it wasn't right to use because most
#of the data is continuous other than brood_size

gil_2019_omit_glmer <-glmer(tel_length ~ cort + sex + brood_size + 
                              (1|mass3) +
                              (1|originalnest_ID)+
                              (1|expnest_ID), 
                            data=gil_2019_omit_new_variable)
warnings()
print(gil_2019_omit_glmer)
?isSingular

#glmer does not produce a model for gaussian, 

#glmer 2 no sex, all random factors 

gil_2019_omit_glmer_2 <-glmer(tel_length ~ cort + 
                                (1|brood_size) + (1|mass3) +
                                (1|originalnest_ID)+
                                (1|expnest_ID),
                              data=gil_2019_omit_new_variable)
print(gil_2019_omit_glmer_2)

#intercept for cort even closer to zero.  I didn't think the model will produce any signicficant values
#worth a shot
#glmer 3 no experimental nest ID

gil_2019_omit_glmer_3 <-glmer(tel_length ~ cort + sex +
                                (1|brood_size) + (1|mass3) +
                                (1|originalnest_ID),
                              data=gil_2019_omit_new_variable)
print(gil_2019_omit_glmer_3)
#cort slightly more negative (neg correlation w TL)
#glmer 4 no original nest ID

gil_2019_omit_glmer_4 <-glmer(tel_length ~ cort + sex +
                                (1|brood_size) + (1|mass3),
                              data=gil_2019_omit_new_variable)
print(gil_2019_omit_glmer_4)
#glmer 5 has interaction terms of cort, brood size, and sex

gil_2019_omit_glmer_5 <-glmer(tel_length ~ cort*brood_size*sex + (1|mass3) + (1|originalnest_ID) + (1|expnest_ID),
                              data=gil_2019_omit_new_variable)
print(gil_2019_omit_glmer_5)


library(MuMIn)
gil_2019_omit_lmer <-lmer(tel_length ~ cort + sex + brood_size +
                            (1|mass3)+
                            (1|originalnest_ID)+(1|expnest_ID), 
                          data=gil_2019_omit_new_variable)
print(gil_2019_omit_lmer)
r.squaredGLMM(gil_2019_omit_lmer)

#ultimately I needed an R^2 value and how I was going to get it was from lmer 

gil_2019_omit_lmer2 <-lmer(tel_length ~ cort*brood_size*sex + (1|mass3)+
                            (1|originalnest_ID)+(1|expnest_ID), 
                          data=gil_2019_omit_new_variable)
print(gil_2019_omit_lmer2)
r.squaredGLMM(gil_2019_omit_lmer2)

#study manipulates brood size, this can be used as an independent variable.  Interested in marginal R^2. 
gil_2019_omit_lmer3 <-lmer(tel_length ~ cort + brood_size + sex
                             + (1|mass3)+
                             (1|originalnest_ID)+(1|expnest_ID), 
                           data=gil_2019_omit_new_variable)
print(gil_2019_omit_lmer3)
r.squaredGLMM(gil_2019_omit_lmer3)

#R2m is never really high, but at least I can incorporate it into my Zr for the effect size 
#try with ln(cort) and then use lmer 

gil_2019_omit_ln_cort <- gil_2019_omit_new_variable %>% mutate(ln_cort = log(cort))
print(gil_2019_omit_ln_cort)

gil_2019_omit_ln_cort_lm <- lm(tel_length ~ cort*sex*brood_size, data = gil_2019_omit_ln_cort )
summary(gil_2019_omit_ln_cort_lm)        

gil_2019_omit_ln_cort_lmer <- lmer(tel_length ~ ln_cort + sex + (1|mass3), data = gil_2019_omit_ln_cort )
summary(gil_2019_omit_ln_cort_lmer)














