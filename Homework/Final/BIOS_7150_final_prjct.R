####BIOS 7300: Final Project####
if(!require(tidyverse))
  install.packages("tidyverse")
if(!require(survival))
  install.packages("survival")
if(!require(geepack))
  install.packages("geepack")
if(!require(MASS))
  install.packages("MASS")
if(!require(MuMIn))
  install.packages("MuMIn")
if(!require(bestglm))
  install.packages("bestglm")
if(!require(car))
  install.packages("car")
if(!require(rms))
  install.packages("rms")
if(!require(ResourceSelection))
  install.packages("ResourceSelection")

####Q1####
#Read in the "takehome_drug.csv" file
q1.data<-read_csv(file=file.choose(), col_names=T)

#convert "drug", "gender" to categorical variables (response can remain "int")
q1.data<-mutate_each(q1.data, funs(factor), drug, gender)
attach(q1.data)

#run the GEE formula
q1.drug<-formula(resp~drug+gender+drug*gender)
q1.gee<-geeglm(q1.drug, id=pt, family=binomial, corstr = "exchangeable")

detach(q1.data)

####Q2####
#Read in "vaccine.csv"
q2.data<-read_csv(file=file.choose(), col_names = TRUE)
attach(q2.data)

##Data Description##
#sample characteristics
n_cases<-sum(satis) #number of cases (discomfort)
n_controls<-length(satis)-n_cases #number of controls
n_vacc1<-length(vaccine[vaccine==1]) #number who received vaccine nasally
n_vacc2<-length(vaccine[vaccine==2]) #number who received vaccine shot
n_race1<-length(race[race==1]) #number who are race 1
n_race2<-length(race[race==2]) #number who are race 2
n_race3<-length(race[race==3]) #number who are race 3
n_race4<-length(race[race==4]) #number who are race 4
n_race9<-length(race[race==9]) #number who are race 4
n_age1<-length(age[age==1]) #number who are race 4
n_age2<-length(age[age==2]) #number who are race 4
n_age3<-length(age[age==3]) #number who are race 4
n_age4<-length(age[age==4]) #number who are race 4
n_age5<-length(age[age==5]) #number who are race 4
n_age6<-length(age[age==6]) #number who are race 4
n_age7<-length(age[age==7]) #number who are race 4
n_male<-length(sex[sex==1]) #number male
n_female<-length(sex[sex==2]) #number female

#get bivariate relationships
vacc1_satis0<-length(satis[vaccine==1 & satis==0])
vacc1_satis1<-length(satis[vaccine==1 & satis==1])
vacc2_satis0<-length(satis[vaccine==2 & satis==0])
vacc2_satis1<-length(satis[vaccine==2 & satis==1])
vacc_vec<-c(vacc1_satis0, vacc1_satis1, vacc2_satis0, vacc2_satis1)

#get the odds ratio for discomfort:comfort (case:control)
ln_vacc_OR<-log(vacc1_satis1/(vacc1_satis0)/(vacc2_satis1/vacc2_satis0))
ln_vacc_stdev<-sqrt(1/vacc_vec[1]+1/vacc_vec[2]+1/vacc_vec[3]+1/vacc_vec[4])
ln_vacc_95CI<-c(ln_vacc_OR-qnorm(.975)*ln_vacc_stdev, 
             ln_vacc_OR+qnorm(.975)*ln_vacc_stdev)
vacc_95CI<-c(exp(ln_vacc_95CI[1]), exp(ln_vacc_95CI[2]))

#transform "race", "sex", "age", "vaccine" into factor class
#the response and stratification variables remain integer
detach(q2.data)
q2.data<-mutate_each(q2.data, funs(factor), race, sex, age, vaccine)

#convert sex =9 to sex =2
q2.data$sex<-Recode(q2.data$sex, "c('2','9')='2'; else='1'")

##Model Selection
#unadjusted effects
#L_2.vaccine<- -2*(q2.vaccine.clogit$loglik[1]-q2.vaccine.clogit$loglik[2])
q2.vaccine.clogit<-clogit(satis ~ vaccine + strata(set) , q2.data)
#L_2.race<- -2*(q2.race.clogit$loglik[1]-q2.race.clogit$loglik[2])
q2.race.clogit<-clogit(satis ~ race + strata(set) , q2.data)
#L_2.race<- -2*(q2.race.clogit$loglik[1]-q2.race.clogit$loglik[2])
q2.sex.clogit<-clogit(satis ~ sex + strata(set) , q2.data)

#matched only on age (essentially FME)
q2.fme.clogit <- clogit(satis ~ vaccine + 
                              race + sex + strata(set) , q2.data)
#saturated model
q2.sat.clogit <- clogit(satis~vaccine+race+sex+vaccine*race+
                          vaccine*sex+race*sex+strata(set), q2.data)

##Model selection
#stepwise selection with just the full main effects
q2.stepfme.clogit<-stepAIC(q2.fme.clogit, direction="both")

#stepwise selection with all two-way interactions
q2.stepintrct.clogit<-stepAIC(q2.sat.clogit, direction="both")

#***Final Model***
#combine levels 3, 4, 9 of race
for (i in 1:length(q2.data$race)){
  if (q2.data$race[i]==4){
    q2.data$race[i]<-3
  } else if(q2.data$race[i]==9){
    q2.data$race[i]<-3
  }
}
q2.data$race<-factor(q2.data$race)

#checking for linear trend with race variable
#contrasts(race)<-contr.poly(5)
#ord.race<-factor(race, levels=c("1", "2", "3", "4", "9"), ordered = T)
# coef exp(coef) se(coef)     z Pr(>|z|)  
# ord.race.L 0.2002   1.2217  0.1999 1.002   0.3166  
# ord.race.Q -0.2660  0.7664  0.1311 -2.029   0.0425*  

##Final model
q2.final.clogit<-clogit(satis~vaccine+race+sex+strata(set), q2.data)
##Plot of predicted log odds
#plot of predicted log odds vs. race (by vaccine status)
p_race<-ggplot()+
  geom_point(aes(x=c("1", "2", "3"), y=c(2.8082, 3.139, 2.6846),
                 colour="Shot"))+
  geom_line(aes(x=c("1", "2", "3"), y=c(2.8082, 3.139, 2.6846)), group=1)+
  geom_point(aes(x=c("1", "2", "3"), y=c(0, 0.3308, -0.1236),
                 colour="Nasal"))+
  geom_line(aes(x=c("1", "2", "3"), y=c(0, 0.3308, -0.1236)), group=2)+
  labs(title="Predicted Log Odds of Discomfort vs. Race (by Vaccine type)",
       y="Predicted Log Odds", x="Race")

#plot of predicted log odds vs. Vaccine type (by Race)
p_vacc<-ggplot()+
  geom_point(aes(x=c("Nasal", "Shot"), y=c(0, 2.8082),
                 colour="Race 1"))+
  geom_line(aes(x=c("Nasal", "Shot"), y=c(0, 2.8082)), group=1)+
  geom_point(aes(x=c("Nasal", "Shot"), y=c(0.3308, 3.139),
                 colour="Race 2"))+
  geom_line(aes(x=c("Nasal", "Shot"), y=c(0.3308, 3.139)), group=2)+
  geom_point(aes(x=c("Nasal", "Shot"), y=c(-0.1236, 2.6846),
                 colour="Race 3"))+
  geom_line(aes(x=c("Nasal", "Shot"), y=c(-0.1236, 2.6846)), group=3)+
  labs(title="Predicted Log Odds of Discomfort vs. Vaccine Status (by Race)",
       y="Predicted Log Odds", x="Vaccine status")

####Q3####
#Read in "framing.dat"
q3.data<-read_table(file=file.choose(), col_names = TRUE)
attach(q3.data)

#convert "frw" into
q3.data<-mutate_each(q3.data, funs(as.integer), frw)

##Data description
n_chdcase<-sum(chd) #number of cases of chd
n_chdcontrols<-length(chd[chd==0]) #number of controls of chd
#age
avg_age<-mean(age)
sd_age<-sd(age)
med_age<-median(age)
#sbp
avg_sbp<-mean(sbp)
sd_sbp<-sd(sbp)
med_sbp<-median(sbp)
#dbp
avg_dbp<-mean(dbp)
sd_dbp<-sd(dbp)
med_dbp<-median(dbp)
#tchol
avg_tchol<-mean(tchol)
sd_tchol<-sd(tchol)
med_tchol<-median(tchol)
#frw
avg_frw<-mean(frw[is.na(frw)==F])
sd_frw<-sd(frw[is.na(frw)==F])
med_frw<-median(frw[is.na(frw)==F])
#cig
avg_cig<-mean(cig[is.na(cig)==F])
sd_cig<-sd(cig[is.na(cig)==F])
med_cig<-median(cig[is.na(cig)==F])

#turn chd into a "factor", as it is a binomial response.
q3.data<-mutate_each(q3.data, funs(factor), chd)


#replace "." with "NA" for missing values in all values in all variables
#loop through each column
for (i in seq_along(q3.data)){
  #loop through each value in each column
  for (j in seq_along(q3.data[[i]])){
    #search for NA first; otherwise, the conditional will fail
    if(is.na(q3.data[[i]][j])==TRUE){
      q3.data[[i]][j]<-NA
    }else if (q3.data[[i]][j]=="."){
      q3.data[[i]][j]<-NA
    } 
  }
}

#create categorical cigarette
for (i in seq_along(q3.data$cig)){
  if (is.na(q3.data$cig[i])==TRUE){
    q3.data$cig[i]<-NA
  } else if (q3.data$cig[i]>10){
    q3.data$cig[i]<-1 #yes; a smoker
  } else if (q3.data$cig[i]<=10){
    q3.data$cig[i]<-0 #no; not a smoker
  }
}
#turn "cig" into a factor
q3.data$cig<-factor(q3.data$cig)

#create categorical variables for sbp and dbp
#sbp
q3.data$fac_sbp<-c()
for (i in seq_along(q3.data$sbp)){
  if (is.na(q3.data$sbp[i])==TRUE){
    q3.data$fac_sbp[i]<-NA
  } else if (q3.data$sbp[i]>=140){
    q3.data$fac_sbp[i]<-1 #yes; sbp hypertensive
  } else if (q3.data$sbp[i]<140){
    q3.data$fac_sbp[i]<-0 #no; sbp normotensive
  }
}
#turn "fac_sbp" into a factor
q3.data$fac_sbp<-factor(q3.data$fac_sbp)

#dbp
q3.data$fac_dbp<-c()
for (i in seq_along(q3.data$dbp)){
  if (is.na(q3.data$dbp[i])==TRUE){
    q3.data$fac_dbp[i]<-NA
  } else if (q3.data$dbp[i]>=90){
    q3.data$fac_dbp[i]<-1 #yes; dbp hypertensive
  } else if (q3.data$dbp[i]<90){
    q3.data$fac_dbp[i]<-0 #no; dbp normotensive
  }
}
#turn "fac_dbp" into a factor
q3.data$fac_dbp<-factor(q3.data$fac_dbp)

detach(q3.data)

###Model Selection###
##Unadjusted effects##
#unadjusted effects: model$null.deviance-model$deviance
q3.age.model<-glm(chd~age, family="binomial")
q3.sbp.model<-glm(chd~sbp, family="binomial")
q3.dbp.model<-glm(chd~dbp, family="binomial")
q3.tchol.model<-glm(chd~tchol, family="binomial")
q3.frw.model<-glm(chd~frw, family="binomial")
q3.cig.model<-glm(chd~cig, family="binomial")
q3.fac_sbp.model<-glm(chd~fac_sbp, family="binomial")
q3.fac_dbp.model<-glm(chd~fac_dbp, family="binomial")

##get FME for adjusted effects
#FME for continuous bp
q3.fme_cont.model<-glm(chd~age+sbp+dbp+tchol+frw+cig, family="binomial")
#FME for categorical bp
q3.fme_fac.model<-glm(chd~age+fac_sbp+fac_dbp+tchol+frw+cig, family="binomial")

#get data from for "bestglm"
bestglm.cont.data<-data.frame(age, sbp, dbp, tchol, frw, cig, chd)%>%
  na.omit()
#get data from for "bestglm"
bestglm.fac.data<-data.frame(age, fac_sbp, fac_dbp, tchol, frw, cig, chd)%>%
  na.omit()

#run best subsets regression
q3bestglm.cont.model<-bestglm(bestglm.cont.data, family=binomial, IC="AIC",
                              method="exhaustive")
#get the Nagelkerke R^2 value
q3.contlrm<-lrm(chd~age+sbp+tchol+cig, data=bestglm.cont.data)
#run best glm
q3bestglm.fac.model<-bestglm(bestglm.fac.data, family=binomial, IC="AIC",
                             method="exhaustive")
#get the Nagelkerke R^2 value
q3.faclrm<-lrm(chd~age+fac_sbp+fac_dbp+tchol+frw+cig, data=bestglm.fac.data)

#develop code for "stepAIC" based on table of effects
q3sat.model<-glm(chd~age+sbp+dbp+tchol+frw+cig+age*sbp+age*dbp+age*tchol
              +age*frw+age*cig+sbp*dbp+sbp*tchol+sbp*frw+sbp*cig+dbp*tchol
              +dbp*tchol+dbp*frw+dbp*cig+tchol*frw+tchol*frw+frw*age,
              family="binomial", data=bestglm.cont.data)

#stepwise regression (this was the final model)
q3.step.model<-stepAIC(q3sat.model, direction="both") 
#confidence intervals for final model
OR_q3.step<-exp(cbind(OR = coef(q3.step.model), confint.default(q3.step.model)))

##Plots of Predicted Log Odds
#plot of predicted lnodds vs. tchol
p_tchol<-ggplot()+
  geom_point(aes(x=c("230", "235", "240"), 
             y=c(13.4119,13.6574,13.8989),colour="dp85"))+
  geom_line(aes(x=c("230", "235", "240"), 
            y=c(13.4119,13.6574,13.8989), group=1))+
  geom_point(aes(x=c("230", "235", "240"),
             y=c(13.2509,13.4881,13.7252), colour="dp90"))+
  geom_line(aes(x=c("230", "235", "240"),
                y=c(13.2509,13.4881,13.7252), group=2))+
  geom_point(aes(x=c("230", "235", "240"), 
             y=c(13.09,13.3208,13.5516), colour="dp95"))+
  geom_line(aes(x=c("230", "235", "240"), 
                y=c(13.09,13.3208,13.5516), group=3))+
  labs(title="Predicted LnOdds of CHD vs. Tot. Cholesterol (by DBP)",
       x="Total cholesterol(mg/dL)", y="Predicted LnOdds")

#plot of predicted lnodds vs. dbp
p_dbp<-ggplot()+
  geom_point(aes(x=c("85", "90", "95"), 
                 y=c(13.4119,13.2509,13.09),colour="tchol230"))+
  geom_line(aes(x=c("85", "90", "95"), 
                y=c(13.4119,13.2509,13.09), group=1))+
  geom_point(aes(x=c("85", "90", "95"),
                 y=c(13.6554,13.4881,13.3208), colour="tchol235"))+
  geom_line(aes(x=c("85", "90", "95"),
                y=c(13.6554,13.4881,13.3208), group=2))+
  geom_point(aes(x=c("85", "90", "95"), 
                 y=c(13.8989,13.7252,13.5516), colour="tchol240"))+
  geom_line(aes(x=c("85", "90", "95"), 
                y=c(13.8989,13.7252,13.5516), group=3))+
  labs(title="Predicted LnOdds of CHD vs. Diastolic BP (by tchol)",
       x="DBP(mmHg)", y="Predicted LnOdds")
  

detach(q3.data)
####Q4####
#Read in the "th1.csv" file
q4.data<-read_csv(file=file.choose(), col_names=T)

#replace "." with "NA" for missing values in all values in all variables
#loop through each column
for (i in seq_along(q4.data)){
  #loop through each value in each column
  for (j in seq_along(q4.data[[i]])){
    #search for NA first; otherwise, the conditional will fail
    if(is.na(q4.data[[i]][j])==TRUE){
      q4.data[[i]][j]<-NA
    }else if (q4.data[[i]][j]=="."){
      q4.data[[i]][j]<-NA
    } 
  }
}

#turn "ER_ELC", "sex", "race", "precat",...
# "compout2", & "comorb" into class factor
q4.data<-mutate_each(q4.data, funs(factor), ER_ELC, sex, race2, precat,
                     compout2, comorb)

#turn BMI into class "numeric"
q4.data<-mutate_each(q4.data, funs(as.numeric), BMI)

#uandjusted effect of "precat" on outcome "compout2"
attach(q4.data)
precat.model<-glm(compout2~precat, family="binomial")

#unadjusted effects of other predictors
BMI.model<-glm(compout2~BMI, family="binomial")
ER_ELC.model<-glm(compout2~ER_ELC, family="binomial")
age.model<-glm(compout2~age, family="binomial")
sex.model<-glm(compout2~sex, family="binomial")
race2.model<-glm(compout2~race2, family="binomial")
comorb.model<-glm(compout2~comorb, family="binomial")

##Adjusted Effects##
#get FME model
q4fme.model<-glm(compout2~precat+BMI+ER_ELC+age+sex+race2+comorb,
                 family="binomial")

##Model Selection
#Best subset regression model selection
q4bestglm.data<-data.frame(precat, BMI, ER_ELC, age, 
                           sex, race2, comorb, compout2)%>%
na.omit()
q4bestglm.model<-bestglm(q4bestglm.data, family=binomial, IC="AIC")

#get saturated model
# q4sat.model<-glm(compout2~precat+BMI+ER_ELC+age+sex+race2+comorb+precat*BMI
#                +precat*ER_ELC+precat*age+precat*sex+precat*race2
#                +precat*comorb+BMI*ER_ELC+BMI*age+BMI*sex+BMI*race2
#                +BMI*comorb+ER_ELC*age+ER_ELC*sex+ER_ELC*race2+ER_ELC*comorb
#                +age*sex+age*race2+age*comorb+sex*race2+sex*comorb
               # +race2*comorb, family="binomial")
#develop stepAIC code based on table of effects and saturated model