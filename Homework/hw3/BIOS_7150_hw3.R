####BIOS 7150 Hw 3####
###Loading Packages####
#rm(list=ls())
if(!require(plyr))
  install.packages("plyr")
if(!require(tidyverse))
  install.packages("tidyverse")
if(!require(stringr))
  install.packages("stringr")
if(!require(forcats))
    install.packages("forcats")
if(!require(aod))
  install.packages("aod")
if(!require(rms))
  install.packages("rms")
if(!require(bestglm))
  install.packages("bestglm")

###Data Transformation####
#read in data
hw3data<-read_csv(file=file.choose(), col_names = T)
hw3data<-as_tibble(hw3data)
 
#replace "." with "NA" for missing values
for (i in seq_along(hw3data$hins)){
  if (hw3data$hins[i]=="."){
    hw3data$hins[i]<-NA
  }  
}

#convert columns to class "factor"
hw3data<-colwise(factor)(hw3data)
attach(hw3data)

###initial model####
hypins_model<-glm(hins~gender+ethnic+agegr+hdlcat+bmicat, family="binomial", data=hw3data)
#summary(hypins_model)

###Testing for trend for age, hdl, and bmi####
contrasts(agegr)<-contr.poly(3)
cont_agegr<-as.factor(agegr)
# .L         .Q
# 1 -7.071068e-01  0.4082483
# 2 -7.850462e-17 -0.8164966
# 3  7.071068e-01  0.4082483
# Levels: 1 2 3
contrasts(hdlcat)<-contr.poly(4)
cont_hdlcat<-as.factor(hdlcat)
# .L   .Q         .C
# . -0.6708204  0.5 -0.2236068
# 1 -0.2236068 -0.5  0.6708204
# 2  0.2236068 -0.5 -0.6708204
# 3  0.6708204  0.5  0.2236068
# Levels: . 1 2 3
contrasts(bmicat)<-contr.poly(4)
cont_bmicat<-as.factor(bmicat)
# .L   .Q         .C
# . -0.6708204  0.5 -0.2236068
# 1 -0.2236068 -0.5  0.6708204
# 2  0.2236068 -0.5 -0.6708204
# 3  0.6708204  0.5  0.2236068
# Levels: . 1 2 3
##Comment out the contrasts for hdl and bmi since there are not significant trends
hypins_ord_model<-glm(hins~gender+ethnic+cont_agegr+cont_hdlcat+cont_bmicat, family="binomial", data=hw3data)
##none of the linear trend effects are significant when the model is run. Therefore,
##having a linear trend does not simplify the model
###"Bestglm"####
#reorder columns to have response var "hypins"
hw3data_bestglm<-hw3data[c("gender", "ethnic", "agegr", "hdlcat", "bmicat",
                           "hins")]%>%
  na.omit()
hypins_bestglm_model<-bestglm(hw3data_bestglm, family=binomial, IC="AIC")
