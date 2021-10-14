
library(meta)
library(metafor)

correlation_data <- read.csv("correlation_data_for_metaanaly.csv")

str(correlation_data)

m.cor <- metacor(r_squared, 
                 sample_size, 
                 data = correlation_data,
                 studlab = correlation_data$author_name,
                 sm = "ZCOR",
                 method.tau = "SJ")
print(m.cor)

#forest plot to represent pooled effect sizes 
#updated 4/6/21
forest(m.cor, col.square = "hotpink2", col.diamond = "coral2") 
  

#pooled effect size Fisher's Z random effects model
#4/6/21
#0.1042 [0.0235; 0.1836]

#heterogeneity 4/6/21
#tau^2 =  tau^2 = 0.0077 [0.0000; 0.0191]
#I^2 = 0.0% [0.0%; 42.6%]
#Cochrane's Q=11.31, p= 0.6615


#heterogeneity..note: I have low heterogeneity, but I am still doing all the steps just in case
#firstly, I will look for outliers by identifying the pooled 95% confidence interval, not sure if I can since not 
#random or fixed effects
#random fit my model better (yes) so I will use random 

#outlier identification
#4/15/21
m.cor$lower.random
0.0234855
m.cor$upper.random
0.1857237

outliers <- find.outliers(m.cor)
print(outliers)

#4/15/21 no studies were recommended for removal

#now to look at the influences of individual studies on the pooled effect sizes, I have to load a couple more packages
#to get the source code to work


library(ggplot2)
library(ggrepel)
library(forcats)
library(dplyr)
library(grid)
library(gridExtra)
library(devtools)

#copy and paste source code again
#this is for influence analysis

inf_analy <- InfluenceAnalysis(x = m.cor, random = TRUE)
list(inf_analy)

plot(inf_analy, "es")
plot(inf_analy, "baujat")

#use list instead of summary

#copy and pasted source code for subgroup analysis for mixed effects: fixed + random effects
#subgroup analysis for time frame
#updated 4/6/21

subgroup_time_random <- update.meta(m.cor, 
                                    byvar=time_frame_revised, 
                                    comb.random = TRUE, 
                                    comb.fixed = FALSE)
summary(subgroup_time_random)
#less_than_1_week   k=1, Fisher's Z=0.0135, [-0.2717; 0.2965] 
#1_to_2_weeks       k=2, Fisher's Z= 0.0959, [-0.1663; 0.3454]
#2_to_3_weeks       k=7, Fisher's Z= 0.0741, [-0.0157; 0.1628] 
#3_to_4_weeks       k=1, Fisher's Z= 0.0957, [-0.2950; 0.4591]
#over_4_weeks       k=4, Fisher's Z= 0.2181, [ 0.0101; 0.4080]

#Between groups  Cochrane's Q =1.86, p = 0.7594
#time subgroup double checked 7/17/21



#subgroup analysis for stressor 
#updated 4/6/21
#rerun 7/17/21 and 7/18/21
#subgroups "species," "telomere_assay," and "study design" added 7/17/21

subgroup_stressor_random <- update.meta(m.cor,
                                        byvar = stressor_type_revised,
                                        comb.random = TRUE, 
                                        comb.fixed = FALSE)

summary(subgroup_stressor_random)
#anthropogenic       k=5, Fisher's Z= 0.1902, [ 0.0059; 0.3621]
#cort_administered   k=3, Fisher's Z= 0.1425, [-0.0320; 0.3085]
#natural             k=7, Fisher's Z= 0.0451, [-0.0388; 0.1284]

#Between groups  Cochrane's Q = 2.56, p=0.2783
#DOUBLE CHECKED 7/18/21

##subgroup analysis for life history stage
#updated 4/6/21
#DOUBLE CHECKED 7/18/21
subgroup_life_history_random <- update.meta(m.cor,
                                          byvar = life_history_stage_revised,
                                          comb.random = TRUE, 
                                          comb.fixed = FALSE)
summary(subgroup_life_history_random)
#adult          k = 3, Fisher's Z = 0.0843, [-0.1183; 0.2800]
#developing k =12, Fisher's Z = 0.1111, [ 0.0185; 0.2019]

#Between groups   Cochrane's Q = 0.06    p = 0.8119

#subgroup analysis for cort assay
#updated 4/6/21
subgroup_cort_assay_random <- update.meta(m.cor,
                                        byvar = cort_assay_revised,
                                        comb.random = TRUE, 
                                        comb.fixed = FALSE)
summary(subgroup_cort_assay_random)
#non_plasma   k=3, Fisher's Z = 0.1161, [-0.0437; 0.2701]
#plasma      k=12, Fisher's Z = 0.1012, [ 0.0061; 0.1945]

#Between groups   Cochrane's Q = 0.03, p = 0.8742

#subgroup analysis for taxa
#updated 4/6/21
subgroup_taxa_random <- update.meta(m.cor,
                                          byvar = taxa_revised,
                                          comb.random = TRUE, 
                                          comb.fixed = FALSE)
summary(subgroup_taxa_random)
#avian      k = 12, Fisher's Z = 0.1012, [ 0.0088; 0.1919]
#non_avian k = 3, Fisher's Z = 0.1183, [-0.0600; 0.2893] 
#Between groups  Cochrane's Q = 0.03, p = 0.8666

#############################################################

#updated subgroups from reviewers 7/18/21 LHZ
#species subgroup
subgroup_species_random <- update.meta(m.cor,
                                    byvar = species,
                                    comb.random = TRUE, 
                                    comb.fixed = FALSE)
#Capreolus capreolus k = 1, Fisher's Z = 0.0993, [-0.2072; 0.3881] 
#Coturnix japonica k = 1, Fisher's Z = 0.0596, [-0.1973; 0.3089] 
#Fregata magnificens  k = 1, Fisher's Z = 0.1306, [-0.1232; 0.3684] 
#Hydrobates pelagicus k = 1, Fisher's Z = 0.4651, [ 0.0857; 0.7267] 
#Parus major k =2, Fisher's Z = 0.0707, [-0.1322; 0.2678]
#Phalacrocorax aristotelis k = 1, Fisher's Z = 0.1852, [-0.0077; 0.3648]
#Rana temporaria k = 1, Fisher's Z = 0.0067, [-0.1962; 0.2090] 
#Rissa tridactyla k = 1, Fisher's Z = 0.0826, [-0.1686; 0.3238] 
#Sterna hirundo k = 1, Fisher's Z = 0.0957, [-0.2950; 0.4591] 
#Sturnus unicolor k = 1, Fisher's Z = 0.0088, [-0.1182; 0.1356]   
#Tachycineta bicolor k= 2, Fisher's Z = 0.1134, [-0.2110; 0.4153]
#Turdus merula k = 1, Fisher's Z = 0.0539, [-0.3004; 0.3952] 
# Welsh pony k = 1, Fisher's Z = 0.2693, [ 0.0252; 0.4831]  

#Between groups Cochrane's Q =  9.27 , p = 0.6797


summary(subgroup_species_random)
#telomere assay subgroup
subgroup_telo_assay_random <- update.meta(m.cor,
                                       byvar = telomere_assay,
                                       comb.random = TRUE, 
                                       comb.fixed = FALSE)

summary(subgroup_telo_assay_random)
#qPCR k = 7, Fisher's Z = 0.1186, [-0.0089; 0.2424]
#TeloTAGGG k = 1, Fisher's Z = 0.1306, [-0.1232; 0.3684]
#TRF k = 7, Fisher's Z = 0.0909, [-0.0409; 0.2197]

#Between groups Cochrane's Q =  0.12, p = 0.9401

#study type subgroup
subgroup_study_type_random <- update.meta(m.cor,
                                       byvar = study_type,
                                       comb.random = TRUE, 
                                       comb.fixed = FALSE)
summary(subgroup_study_type_random)
#cross_sectional k =  5, Fisher's Z =  0.1687, [-0.0219; 0.3474]
#repeated_measures k = 2, Fisher's Z = 0.0271, [-0.1336; 0.1864]
#within_individual k =  8, Fisher's Z = 0.0984 [ 0.0040; 0.1910]

#Between groups Cochrane's Q = 1.27, p = 0.5289


############################################################

#meta-regression 4/6/21
meta_regression_pub_year <-metareg(m.cor, publication_year)
summary(meta_regression_pub_year)
#Test of Moderators (coefficient 2):
#QM(df = 1) = 1.252, p-val = 0.2632
        
        
#funnel plot
funnel(m.cor, pch = 16, cex = 1.5, col = "hotpink", studlab = TRUE,  
        )

#publication bias

#after creating the funnel plot, I must again code copy and paste to get a statistical test for publication bias called eggers test
#here we go

eggers_test <- eggers.test(x = m.cor)
list(eggers_test)
#intercept = 1.420616, [0.3753223, 2.465909], p = 0.02064949




# Risk of Bias
library(robvis)
#risk of bias requires use of Cochrane tool 
#will come back to this after power analysis

#power analysis 
library(ggplot2)
#d is the hypothesizes effect size
#k is the number of studies
#n1 number of individuals in ctrl group
#n2 number of individuals in the treatment group 
#p= 0.05 as usual 

power.analysis(d=0.3, 
               k=14, 
               n1=30, 
               n2=30, 
               p=0.05)
#d is high of course, but when I started I thought the effect size would be more than 0
#thoughts?
#smd power analysis 
#cannot find fisher's Z analysis 
#however, many studies have treatment and control groups
#ask David
