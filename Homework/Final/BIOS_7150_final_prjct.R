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

#transform "race", "sex", "age", "vaccine" into factor class
#the response and stratification variables remain integer
q2.data<-mutate_each(q2.data, funs(factor), race, sex, age, vaccine)

#matched only on age
q2.unmatch.clogit <- clogit(satis ~ vaccine + 
                              race + sex + strata(set) , q2.data)
#matched on age, sex, and race
q2.match.clogit <- clogit(satis ~ vaccine + strata(set) , q2.data)

detach(q2.data)

####Q3####
#Read in "framing.dat"
q3.data<-read_table(file=file.choose(), col_names = TRUE)
# attach(q3.data)

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

###Model Selection###
##Unadjusted effects##
attach(q3.data)

#unadjusted effects: model$null.deviance-model$deviance
q3.age.model<-glm(chd~age, family="binomial")
q3.age.model<-glm(chd~sbp, family="binomial")
q3.age.model<-glm(chd~dbp, family="binomial")
