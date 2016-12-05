####BIOS 7150: Hw4####
if(!require(tidyverse))
  install.packages("tidyverse")
if(!require(survival))
  install.packages("survival")
if(!require(Epi))
  install.packages("Epi")
if(!require(glmulti))
  install.packages("glmulti")
if(!require(MASS))
  install.packages("MASS")

#load the melanoma data
hw4.data<-read_csv(file=file.choose(), col_names=T)


#transform the categorical variables to class "factor"...
#Note! for the "clogit" function in "survival", the outcome and ...
#stratification variable #cannot be class factor!!! Otherwise, we will throw ...
#an error: #"Cox model doesn't support 'mright' survival data"...
#This issue is not present with the "clogistic" function from the "Epi" package
hw4.data<-mutate_each(hw4.data, funs(factor), skin, hair, eyes,
                        freckles, acute, chronic)
attach(hw4.data)

#check for appropriateness of ordinal simplification in ordinal categorical...
#variables "freckles", "acute", and "chronic"
contrasts(hw4.data$freckles) <- contr.poly(3)
ord.freckles<-factor(hw4.data$freckles,
                     levels = c("1", "2", "3"), ordered = T)
contrasts(hw4.data$acute) <- contr.poly(4)
ord.acute<-factor(hw4.data$acute,
                  levels = c("1", "2", "3", "4"), ordered = T)
contrasts(hw4.data$chronic) <- contr.poly(4)
ord.chronic<-factor(hw4.data$chronic, 
                    levels = c("1", "2", "3", "4"), ordered = T)
#append new 
hw4.data<-add_column(hw4.data, ord.freckles, ord.acute, ord.chronic)

####Model Selection####
#Null model
L_2.null<-length(hw4.data$casecon)*log(0.5)

#get the unadjusted effects of each predictor
skin.logit<-clogit(casecon~skin+strata(pair))
hair.logit<-clogit(casecon~hair+strata(pair))
eyes.logit<-clogit(casecon~eyes+strata(pair))
ord.freckles.logit<-clogit(casecon~ord.freckles+strata(pair))
ord.acute.logit<-clogit(casecon~ord.acute+strata(pair))
ord.chronic.logit<-clogit(casecon~ord.chronic+strata(pair))
nvsmall.logit<-clogit(casecon~ord.chronic+strata(pair))
nvlarge.logit<-clogit(casecon~nvlarge+strata(pair))
nvtot.logit<-clogit(casecon~nvtot+strata(pair))
ant15.logit<-clogit(casecon~ant15+strata(pair))

#Log-likelihoods of the unadjusted models
L_2.skin<-logLik(skin.logit)
L_2.hair<-logLik(hair.logit)
L_2.eyes<-logLik(eyes.logit)
L_2.ord.freckles<-logLik(ord.freckles.logit)
L_2.ord.acute<-logLik(ord.acute.logit)
L_2.ord.chronic<-logLik(ord.chronic.logit)
L_2.nvsmall<-logLik(nvsmall.logit)
L_2.nvlarge<-logLik(nvlarge.logit)
L_2.nvtot<-logLik(nvtot.logit)
L_2.ant15<-logLik(ant15.logit)

##Full Main effects model
fme.logit<-clogit(casecon~skin+hair+eyes+ord.freckles+ord.acute+ord.chronic+
                    nvsmall+nvlarge+nvtot+ant15+strata(pair), method="exact",
                  data=hw4.data)

hw4.data<-mutate_each(hw4.data, funs(as.numeric), ord.freckles)

#StepWise Regression for the model
hw4.step=stepAIC(fme.logit, direction="both")

#forward Regression for the model
hw4.forward=stepAIC(fme.logit, direction="forward")

#forward Regression for the model
hw4.backward=stepAIC(fme.logit, direction="backward")

#Best subset regression for the model
# hw4.glmulti<-glmulti(Surv(rep(1, 200L), casecon)~skin+hair+eyes+
#                        ord.chronic+nvsmall+nvlarge+nvtot+ant15+strata(pair),
#                      data=hw4.data,
#                      level=2, method="h",
#                      crit="aic",
#                      confsetsize = 5,
#                      plotty=F, report=F,
#                      fitfunction = "coxph")