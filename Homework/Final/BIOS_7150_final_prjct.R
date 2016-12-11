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
attach(q2.data)

#convert sex =9 to sex =2
for(i in seq_along(q2.data$sex)){
  if (q2.data$sex[i]==9){
    q2.data$sex[i]==2
  }
}

#matched only on age
q2.unmatch.clogit <- clogit(satis ~ vaccine + 
                              race + sex + strata(set) , q2.data)
#matched on age, sex, and race
q2.match.clogit <- clogit(satis ~ vaccine + strata(set) , q2.data)

##Model selection
#Develop code for stepAIC based on table of effects 

detach(q2.data)

####Q3####
#Read in "framing.dat"
q3.data<-read_table(file=file.choose(), col_names = TRUE)
# attach(q3.data)

#turn chd into a "factor", as it is a binomial response.
q3.data<-mutate_each(q3.data, funs(factor), chd)
#convert "frw" into
q3.data<-mutate_each(q3.data, funs(as.integer), frw)

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

###Model Selection###
##Unadjusted effects##
attach(q3.data)

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
q3bestglm.cont.model<-bestglm(bestglm.cont.data, family=binomial, IC="AIC")
#run best glm
q3bestglm.fac.model<-bestglm(bestglm.fac.data, family=binomial, IC="AIC")

#develop code for "stepAIC" based on table of effects
q3sat.model<-(chd~age+sbp+dbp+tchol+frw+cig+age*sbp+age*dbp+age*tchol
              +age*frw+age*cig+sbp*dbp+sbp*tchol+sbp*frw+sbp*cig+dbp*tchol
              +dbp*tchol+dbp*frw+dbp*cig+tchol*frw+tchol*frw+frw*age, 
              family="binomial")

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