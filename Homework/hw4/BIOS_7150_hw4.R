####BIOS 7150: Hw4####
if(!require(tidyverse))
  install.packages("tidyverse")
if(!require(survival))
  install.packages("survival")
if(!require(Epi))
  install.packages("Epi")
if(!require(glmulti))
  install.packages("glmulti")

#load the melanoma data
#hw4.data<-read_csv(file=file.choose(), col_names=T)
hw4.tibble<-as_tibble(hw4.data)


#transform the categorical variables to class "factor"...
#Note! for the "clogit" function in "survival", the outcome and ...
#stratification variable #cannot be class factor!!! Otherwise, we will throw ...
#an error: #"Cox model doesn't support 'mright' survival data"...
#This issue is not present with the "clogistic" function from the "Epi" package
hw4.tibble<-mutate_each(hw4.tibble, funs(factor), skin, hair, eyes,
                        freckles, acute, chronic)
attach(hw4.tibble)

#check for appropriateness of ordinal simplification in ordinal categorical...
#variables "freckles", "acute", and "chronic"
contrasts(hw4.tibble$freckles) <- contr.poly(3)
ord.freckles<-factor(hw4.tibble$freckles,
                     levels = c("1", "2", "3"), ordered = T)
contrasts(hw4.tibble$acute) <- contr.poly(4)
ord.acute<-factor(hw4.tibble$acute,
                  levels = c("1", "2", "3", "4"), ordered = T)
contrasts(hw4.tibble$chronic) <- contr.poly(4)
ord.chronic<-factor(hw4.tibble$chronic, 
                    levels = c("1", "2", "3", "4"), ordered = T)
#append new 
hw4.tibble<-add_column(hw4.tibble, ord.freckles, ord.acute, ord.chronic)

#Full Main effects model
hw4.logit<-clogit(casecon~skin+hair+eyes+ord.freckles+ord.acute+ord.chronic+
                    nvsmall+nvlarge+nvtot+ant15+strata(pair))
hw4.cox<-coxph(formula = Surv(rep(1, 200L), casecon) ~ skin + hair + eyes + 
                 ord.freckles + ord.acute + ord.chronic + nvsmall + nvlarge + 
                 nvtot + ant15 + strata(pair), method = "exact")
#Best subset regression for the model
hw4.glmulti<-glmulti(hw4.logit, data=hw4.tibble, level=2, method="h",
                     crit="aic",
                     confsetsize = 5,
                     plotty=F, report=F,
                     fitfunction = "coxph",
                     family="binomial")

