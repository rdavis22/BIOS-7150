####BIOS 7150 Hw2####
#rm(list=ls())
library(readr)
library(dplyr)
library(effects)
library(car)
library(ggplot2)

#Read in hw2.dat
#raw_hw2data<-read.table(file=file.choose(), header=T)


#relevel raw_hw2data to have "Yes" as first level of "Breast_Cancer" variable and... 
#"Premenopausal" as first level of "Menopausal_Status" variable... 
#Base R 'read.table' puts levels in alphabetical order,... 
#so "No" and "Postmenopausal" levels would normally come first.
raw_hw2data$Breast_Cancer<-relevel(raw_hw2data$Breast_Cancer, "Yes", c("Yes", "No"))
raw_hw2data$Menopausal_Status<-relevel(raw_hw2data$Menopausal_Status, "Premenopausal",
                                       c("Premenopausal", "Postmenopausal"))
#make sure all variables are factors
raw_hw2data$Quartile_biomarker<-as.factor(raw_hw2data$Quartile_biomarker)
attach(raw_hw2data)

#Concatenate by Count variable and index over the other 3 variables to create...
#a 2X4X2 contingency table of "Menopausal_Status"X"Quartile_biomarker"X"Breast_cancer"
#hw2data<-tapply(Count, list(Menopausal_Status, Quartile_biomarker, Breast_Cancer), c)
hw2data<-xtabs(Count~., data=raw_hw2data) #Alternate method of getting contingency table
#detach(raw_hw2data)

#assign Variable names to newly created contingency table.
names(dimnames(hw2data))<-c("Menopausal_Status", "Quartile_biomarker", "Breast_Cancer")
#Create "flattened contingency table" for visualization
hw2_table<-ftable(hw2data, row.vars=c("Menopausal_Status", "Quartile_biomarker"), col.vars = "Breast_Cancer")
print(hw2_table)

####Regression Analysis####
##Part A: picking a regression model
#Get the null model
hw2_null <- glm(Breast_Cancer~ 1, weights=Count, data=raw_hw2data, family=binomial)
#Get the saturated model
hw2_sat = glm(Breast_Cancer ~ Menopausal_Status+Quartile_biomarker+
                Menopausal_Status*Quartile_biomarker, weights = Count,
              data=raw_hw2data, family=binomial)
#forward modeling procedure
hw2_forward = step(hw2_null, scope=list(lower=formula(hw2_null),
                                        upper=formula(hw2_sat)), 
                   direction="forward")
#backwards modeling procedure
hw2_backwards = step(hw2_sat)
#stepwise (non-direct) modeling
hw2_stepwise = step(hw2_null, list(lower=formula(hw2_null),
                                    upper=formula(hw2_sat)), 
                     direction="both",trace=0)

##Part B: Displaying Information for Effects Table
#L^2 of null=5594.6, 15 df
#L^2 of selected model=5496.3, 11 df
#L^2 of saturated model=5493.1, 8 df
hw2_Menopausal_Status<-glm(Breast_Cancer~Menopausal_Status, data=raw_hw2data,
                           family=binomial, weights=Count)
L_2_Menopausal_Status<--2*logLik(hw2_Menopausal_Status) #L^2=5508.3, 2 df
hw2_Quartile_biomarker<-glm(Breast_Cancer~Quartile_biomarker, data=raw_hw2data,
                           family=binomial, weights=Count)
L_2_Quartile_biomarker<--2*logLik(hw2_Quartile_biomarker) #L^2=5581.7, 4 df
hw2_FME<-glm(Breast_Cancer~Menopausal_Status+Quartile_biomarker, data=raw_hw2data,
             family=binomial, weights=Count)
L_2_FME<--2*logLik(hw2_FME)#L^2=5496.3, 5 df

##Part C: Plotting Log odds
#new_hw2data<-with(raw_hw2data, data.frame(Count=rep(seq(from=75, to=450, length.out=100), 4), Menopausal_Status, Quartile_biomarker=factor(rep(1:4, each=100))))
#new2_hw2data <- cbind(new_hw2data, predict(hw2_forward, newdata = new_hw2data, se=TRUE))
# new2_hw2data <- within(new2_hw2data, {
#   PredictedProbability <- plogis(fit)
#   LL <- plogis(fit - (1.96 * se.fit))
#   UL <- plogis(fit + (1.96 * se.fit))
# })
# ggplot(new2_hw2data, aes(x = Menopausal_Status, y = Predicted Probability)) +
#   geom_ribbon(aes(ymin = LL, ymax = UL, fill = Quartile_biomarker), alpha = .2) +
#   geom_line(aes(colour = Quartile_biomarker), size=1)
plot(predict.glm(hw2_forward)) #need to clean up

#Part D: Goodness of Fit statistic
L_2_sat<--2*logLik(hw2_sat)
L_2_forward<--2*logLik(hw2_forward)
hw2_GoF<-L_2_forward-L_2_sat#hw2_GoF=3.19, df=4(#significant coeffs from L_2_sat-#significant coeffs from L_2_forward)