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
##***Eventually, convert to loops

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
#since there are two levels for the response ("case" or "control")
L_2.skin <- -2*(skin.logit$loglik[1]-skin.logit$loglik[2])
L_2.hair <- -2*(hair.logit$loglik[1]-hair.logit$loglik[2])
L_2.eyes <- -2*(eyes.logit$loglik[1]-eyes.logit$loglik[2])
L_2.ord.freckles <- -2*(ord.freckles.logit$loglik[1]-
                          ord.freckles.logit$loglik[2])
L_2.ord.acute <- -2*(ord.acute.logit$loglik[1]-ord.acute.logit$loglik[2])
L_2.ord.chronic <- -2*(ord.chronic.logit$loglik[1]
                       -ord.chronic.logit$loglik[2])
L_2.nvsmall <- -2*(nvsmall.logit$loglik[1]-nvsmall.logit$loglik[2])
L_2.nvlarge <- -2*(nvlarge.logit$loglik[1]-nvlarge.logit$loglik[2])
L_2.nvtot <- -2*(nvtot.logit$loglik[1]-nvtot.logit$loglik[2])
L_2.ant15 <- -2*(ant15.logit$loglik[1]-ant15.logit$loglik[2])

#Full Main effects model
fme.logit<-clogit(casecon~skin+hair+eyes+ord.freckles+ord.acute+ord.chronic+
                    nvsmall+nvlarge+nvtot+ant15+strata(pair), method="exact",
                  data=hw4.data)

#purposeful selection model (from p-values of unadjusted effects)
purpose.logit<-clogit(casecon~hair+ord.freckles+nvlarge+ant15+strata(pair),
                      method="exact", data=hw4.data)

#transform
hw4.data<-mutate_each(hw4.data, funs(as.numeric), ord.freckles)

#StepWise Regression for the model
hw4.step<-stepAIC(fme.logit, direction="both")
hw4purp.step<-stepAIC(purpose.logit, direction="both")

#forward Regression for the model
hw4.forward<-stepAIC(fme.logit, direction="forward")

#forward Regression for the model
hw4.backward<-stepAIC(fme.logit, direction="backward")

####Plot of Predicted Log Odds: for "freckles"####
##building data frame##
freck_coef<-hw4.step$coefficients[names(hw4.step$coefficients)=='ord.freckles']
vec_freck<-c(freck_coef, freck_coef*2, freck_coef*3)
vec_freck_name<-c("1=many", "2=some", "3=none")
freck.data<-tibble(vec_freck, vec_freck_name)

##Building the plot##
p_freck<-ggplot()+
  geom_point(aes(x=vec_freck_name, y=vec_freck))+
  geom_line(aes(x=vec_freck_name, y=vec_freck), group=1)+
  labs(title="Predicted Log Odds for Melanoma vs. Number of Freckles",
       y="Predicted Log Odds")+
  xlab("Number of Freckles")


# blarg<-stepAIC(clogit(casecon~as.numeric(ord.freckles)+nvlarge+ant15+
#                         as.numeric(ord.freckles)*nvlarge+
#                         as.numeric(ord.freckles)*ant15+ant15*nvlarge), 
#                direction="both")

#interaction with 
#Best subset regression for the model
# hw4.glmulti<-glmulti(Surv(rep(1, 200L), casecon)~skin+hair+eyes+
#                        ord.chronic+nvsmall+nvlarge+nvtot+ant15+strata(pair),
#                      data=hw4.data,
#                      level=2, method="h",
#                      crit="aic",
#                      confsetsize = 5,
#                      plotty=F, report=F,
#                      fitfunction = "coxph")