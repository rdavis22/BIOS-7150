####BIOS 7150 Hw 3####
###Loading Packages####
#rm(list=ls())
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
if(!require(multcomp))
  install.packages("multcomp")


####Q1 Parts 1=3: Finding the Best model####
###Data Transformation###
#read in data
hw3data<-read_csv(file=file.choose(), col_names = T)
hw3data<-as_tibble(hw3data)
 
#replace "." with "NA" for missing values in all values in all variables
#loop through each column
for (i in seq_along(hw3data)){
  #loop through each value in each column
  for (j in seq_along(hw3data[[i]])){
    if (hw3data[[i]][j]=="."){
      hw3data[[i]][j]<-NA
    }
  }
}


#convert columns to class "factor"
hw3data<-mutate_each(hw3data, funs(factor), gender, ethnic, hins, agegr,
                     hdlcat, bmicat)
attach(hw3data)

###initial model###
##get unadjusted effects models
gender.model<-glm(hins~gender, family="binomial")
#unadjusted effect: gender.model$null.deviance-gender.model$deviance=0.4590, df=1 
ethnic.model<-glm(hins~ethnic, family="binomial")
#unadjusted effect: ethnic.model$null.deviance-ethnic.model$deviance=26.8743, df=1
agegr.model<-glm(hins~agegr, family="binomial")
#unadjusted effect: agegr.model$null.deviance-agegr.model$deviance=0.5436, df=2
hdlcat.model<-glm(hins~hdlcat, family="binomial")
#unadjuste effect: hdlcat.model$null.deviance-hdlcat.model$deviance=33.5768, df=2
bmicat.model<-glm(hins~bmicat, family="binomial")
#bmicat.model$null.deviance-bmicat.model$deviance=283.1284


hypins.model<-glm(hins~gender+ethnic+agegr+hdlcat+bmicat, family="binomial", data=hw3data)
#summary(hypins_model)

###Testing for trend for age, hdl, and bmi###
contrasts(agegr)<-contr.poly(3)
int.agegr<-factor(agegr,
                  levels = c("1", "2", "3"), ordered = T)
# .L         .Q
# 1 -7.071068e-01  0.4082483
# 2 -7.850462e-17 -0.8164966
# 3  7.071068e-01  0.4082483
# Levels: 1 2 3
contrasts(hdlcat)<-contr.poly(3)
int.hdlcat<-factor(hdlcat,
                   levels = c("1", "2", "3"), ordered = T)
# .L         .Q
# 1 -7.071068e-01  0.4082483
# 2 -7.850462e-17 -0.8164966
# 3  7.071068e-01  0.4082483
# Levels: 1 2 3
contrasts(bmicat)<-contr.poly(3)
int.bmicat<-factor(bmicat,
                   levels = c("1", "2", "3"), ordered = T)
# .L         .Q
# 1 -7.071068e-01  0.4082483
# 2 -7.850462e-17 -0.8164966
# 3  7.071068e-01  0.4082483
# Levels: 1 2 3
##Comment out the contrasts for hdl and bmi since there are not significant trends
hypins_ord.model<-glm(hins~gender+ethnic+int.agegr+int.hdlcat+int.bmicat, family="binomial", data=hw3data)
#cont_agegr.L==>Pr(>|z|)=0.185
#cont_hdlcat.L==>Pr(>|z|)=3.34e-5 (.Q=0.347)
#cont_bmicat.L==>Pr(>|z|)=<2e-16 (.Q=0.446)
###"Bestglm"###
#reorder columns to have response var "hypins" in last colum for "bestglm"
detach(hw3data)
hw3data_bestglm<-data.frame(hw3data$gender, hw3data$ethnic, hw3data$agegr,
                            int.hdlcat, int.bmicat, hw3data$hins)%>%
  na.omit()
#rename the variables of the data frame
names(hw3data_bestglm)<-c("gender", "ethnic", "agegr", "hdlcat", "bmicat"
                          , "hins")
#run best glm
hypins_bestglm.model<-bestglm(hw3data_bestglm, family=binomial, IC="AIC")
attach(hw3data_bestglm)
#best model
best_final.model<-glm(hins~ethnic+hdlcat+bmicat, family=binomial, data=hw3data)

###Built-in R "step" function model selection###
#null model
hw3_null.model <- glm(hins~ 1, data=hw3data_bestglm, family=binomial)
#saturated model
hw3_sat.model<-glm(hins~gender+ethnic+agegr+hdlcat+bmicat+gender*ethnic+
                     gender*agegr+gender*hdlcat+gender*bmicat+ethnic*agegr+
                     ethnic*hdlcat+ethnic*bmicat+agegr*hdlcat+agegr*bmicat+
                     hdlcat*bmicat, data=hw3data_bestglm, family=binomial)
#forward selection
hw3_forward.model <- step(hw3_null.model, scope=list(lower=formula(hw3_null.model),
                                        upper=formula(hw3_sat.model)),
                   direction="forward")
#backward selection
hw3_backwards.model <- step(hw3_sat.model)
#stepwise selection
hw3_step.model <- step(hw3_null.model, list(lower=formula(hw3_null.model),
                                      upper=formula(hw3_sat.model)),
                       direction="both",trace=0)

###Q1 Part 4: Odds Ratios and Plots of predicted log odds####
##Get pairwise comparisons##
#Odds Ratios for multiple comparison test of "ethnic"
K1 <- glht(best_final.model, mcp(ethnic = "Tukey"))$linfct
#Odds Ratios for multiple comparison test of "hdlcat"
K2 <- glht(best_final.model, mcp(hdlcat = "Tukey"))$linfct
#Odds Ratios for multiple comparison test of "bmicat"
K3 <- glht(best_final.model, mcp(bmicat = "Tukey"))$linfct
#All pairwise comparison tests
All_OR <- glht(best_final.model, linfct = rbind(K1, K2, K3))
#Get Confidence Intervals
All_OR_cint <-confint(All_OR)
detach(hw3data_bestglm)

##Plots of predicted log odds##
#predicted log odds of the model
logodds<-predict.glm(best_final.model)
#predicted log odds of ethnic (2 levels) and hdlcat (3 levels) (bmicat=2)
pred_ethn1_hdl<-c(-2.7014+2.5104-0.8629,-2.7014+2.5104+(2*-0.8629),
                  -2.7014+2.5104+(3*-0.8629))
pred_ethn2_hdl<-c(-2.7014+2.5104+0.9467+-0.8629,-2.7014+2.5104+0.9467+(2*-0.8629),
                  -2.7014+2.5104+0.9467+(3*-0.8629))
#predicted log odds of ethnic and bmicat (hdlcat=2)
pred_ethn1_bmi<-c(-2.7014+-0.8629+2.5104,-2.7014+-0.8629+(2*2.5104),
                  -2.7014+-0.8629+(3*2.5104))
pred_ethn2_bmi<-c(-2.7014+-0.8629+0.9467+2.5104,-2.7014+-0.8629+0.9467+(2*2.5104),
                   -2.7014+-0.8629+0.9467+(3*2.5104))
#create dataframes for ggplot2
names(pred_ethn1_hdl)<-c("hdlcat1", "hdlcat2", "hdlcat3")
names(pred_ethn1_bmi)<-c("bmicat1", "bmicat2", "bmicat3")
ethnhdl.data<-data.frame(pred_ethn1_hdl, pred_ethn2_hdl,
                         names(pred_ethn1_hdl))
ethnbmi.data<-data.frame(pred_ethn1_bmi, pred_ethn2_bmi,
                         names(pred_ethn1_bmi))
#Plots for Predicted Log Odds#
#Plot of predicted log odds for ethnicities vs. hdl
p_hdl<-ggplot(ethnhdl.data)+
  #add ethnic 1, hdl log odds points to plot
  geom_point(aes(x=names.pred_ethn1_hdl., y=pred_ethn1_hdl,
                 colour="Whites"), size=2.5)+
  geom_point(aes(x=names.pred_ethn1_hdl., y=pred_ethn2_hdl,
                 colour="AA"), size=2.5)+
  #add a connecting line between points to plot
  geom_line(aes(x=names.pred_ethn1_hdl., y=pred_ethn1_hdl), group=1)+
  geom_line(aes(x=names.pred_ethn1_hdl., y=pred_ethn2_hdl), group=2)+
  #add axis and main title
  labs(title="Log odds for Hyperinsulinemia by HDL in Whites vs. AA (bmicat=2)",
       x="HDL", y="Predicted Log Odds")
p_bmi<-ggplot(ethnbmi.data)+
  #add postmenopausal log odds points to plot
  geom_point(aes(x=names.pred_ethn1_bmi., y=pred_ethn1_bmi,
                 colour="Whites"), size=2.5)+
  geom_point(aes(x=names.pred_ethn1_bmi., y=pred_ethn2_bmi,
                 colour="AA"), size=2.5)+
  #add a connecting line between points to plot
  geom_line(aes(x=names.pred_ethn1_bmi., y=pred_ethn1_bmi), group=1)+
  geom_line(aes(x=names.pred_ethn1_bmi., y=pred_ethn2_bmi), group=2)+
  #add axis and main title
  labs(title="Log odds for Hyperinsulinemia by BMI in Whites vs. AA (hdlcat=2)",
       x="BMI", y="Predicted Log Odds")