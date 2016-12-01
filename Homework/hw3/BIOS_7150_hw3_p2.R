##Hw3 Problem 2####
#rm(list=ls())
if(!require(tidyverse))
  install.packages("tidyverse")
if(!require(stringr))
  install.packages("stringr")
if(!require(multcomp))
  install.packages("multcomp")
if(!require(aod))
  install.packages("aod")
if(!require(Rcpp))
  install.packages("Rcpp")

#read in Mycoplasma Genitalium data
mg.data<-read_table(file = file.choose(), col_names = TRUE)
#transform all columns except "Count" (needs to be int) into class "factor"
for (i in seq_along(grep("Count", names(mg.data), invert=T))){
  mg.data[[i]]<-as.factor(mg.data[[i]])
}
attach(mg.data)
#get the initial model with age as an ordinal predictor
hw3_p2.model<-glm(mg~num_part+trich+age+num_part*trich,
                  data=mg.data, weights = Count, family=binomial)

#transform age into an interval predictor and see if it fits
contrasts(age)<-contr.poly(3)
ord.age<-factor(mg.data$age,
                levels = c("1", "2", "3"), ordered = T)
# attr(,"contrasts")
# .L         .Q
# 1 -7.071068e-01  0.4082483
# 2 -7.850462e-17 -0.8164966
# 3  7.071068e-01  0.4082483
# Levels: 1 2 3

#get the model with interval coded covariate for bmi
hw3_p2_ord.model<-glm(mg~num_part+trich+ord.age+num_part*trich,
                      data=mg.data, weights = Count, family=binomial)
#get the saturated model for GoF comparion
hw3_p2_sat.model<-glm(mg~num_part+trich+age+num_part*trich+num_part*age+trich*age,
                      weights=Count, data=mg.data, family=binomial)

#get odds ratios
#create an interaction term column for the data
mg.data$num_part.trich<-interaction(mg.data$num_part, mg.data$trich)
#run the model again with the interaction column predictor
intrct.model<-glm(mg~ord.age+num_part.trich, 
                  data=mg.data, weight=Count, family=binomial)

#get the matrix of coefficients using the "cont_age" and interaction term
K1<-glht(intrct.model, mcp(ord.age = "Tukey"))$linfct #only does dummy coded
K2<-glht(intrct.model, mcp(num_part.trich = "Tukey"))$linfct
All_OR <- glht(intrct.model, linfct = rbind(K1, K2))
All_OR_cint <-confint(All_OR)

####Plots of predicted log Odds####
## 1) Age on X-axis
#get the predicted log odds (trich=0)
# lodds_model<-predict.glm(intrct.model)
# lodds_num0_part0<-lodds_model[seq(from=1, to=6, by=2)]
# lodds_num0_part1<-lodds_model[seq(from=7, to=12, by=2)]
# lodds_num1_part0<-lodds_model[seq(from=13, to=18, by=2)]
# lodds_num1_part1<-lodds_model[seq(from=19, to=24, by=2)]
lodds_age_num_trich_11<-c(-1.3881, -2.2474, -3.1067)
lodds_age_num_trich_10<-c(-2.3959, -3.2552, -4.1145)
lodds_age_num_trich_01<-c(-3.7312, -4.5905, -5.4498)
lodds_age_num_trich_00<-c(-3.1535, -4.0128, -4.8721)
names(lodds_age_num_trich_11)<-c("age1", "age2", "age3")

num_part_age.data<-data.frame(lodds_age_num_trich_00, lodds_age_num_trich_01,
                          lodds_age_num_trich_10, lodds_age_num_trich_11,
                          names(lodds_age_num_trich_11))

p_age<-ggplot(num_part_age.data)+
  #add ethnic 1, hdl log odds points to plot
  geom_point(aes(x=names.lodds_age_num_trich_11., y=lodds_age_num_trich_00,
                 colour="num0trich0"), size=2.5)+
  geom_point(aes(x=names.lodds_age_num_trich_11., y=lodds_age_num_trich_01,
                 colour="num0trich1"), size=2.5)+
  geom_point(aes(x=names.lodds_age_num_trich_11., y=lodds_age_num_trich_10,
                 colour="num1trich0"), size=2.5)+
  geom_point(aes(x=names.lodds_age_num_trich_11., y=lodds_age_num_trich_11,
                 colour="num1trich1"), size=2.5)+
#add a connecting line between points to plot
  geom_line(aes(x=names.lodds_age_num_trich_11., y=lodds_age_num_trich_00), group=1)+
  geom_line(aes(x=names.lodds_age_num_trich_11., y=lodds_age_num_trich_01), group=2)+
  geom_line(aes(x=names.lodds_age_num_trich_11., y=lodds_age_num_trich_10), group=3)+
  geom_line(aes(x=names.lodds_age_num_trich_11., y=lodds_age_num_trich_11), group=4)+
  #add axis and main title
  labs(title="Log odds for Number of Part. and Trich vs. Age",
       x="Age", y="Predicted Log Odds")

## 2) Num_part on X-axis
lodds_num01_trich0<-c(-3.1493, -2.3959) #trich 0
lodds_num01_trich1<-c(-3.727, -1.3839) #trich 1
names(lodds_num01_trich1)<-c("num_part0", "num_part1")
num_part.data<-data.frame(lodds_num01_trich0, lodds_num01_trich1,
                          names(lodds_num01_trich1))
p_num_part<-ggplot(num_part.data)+
  geom_point(aes(x=names.lodds_num01_trich1., y=lodds_num01_trich0,
                 colour="trich0"), size=2.5)+
  geom_point(aes(x=names.lodds_num01_trich1., y=lodds_num01_trich1,
                 colour="trich1"), size=2.5)+
  geom_line(aes(x=names.lodds_num01_trich1., y=lodds_num01_trich0), group=1)+
  geom_line(aes(x=names.lodds_num01_trich1., y=lodds_num01_trich1), group=2)+
  labs(title="Log odds for Trichomonas Status vs. Number of Partners",
       x="Number of Partners", y="Predicted Log Odds")