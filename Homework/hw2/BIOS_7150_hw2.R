####BIOS 7150 Hw2####
#rm(list=ls())
if(!require(readr))
  install.packages("readr")
if(!require(dplyr))
  install.packages("dplyr")
if(!require(effects))
  install.packages("effects")
if(!require(car))
  install.packages("car")
if(!require(ggplot2))
  install.packages("ggplot2")
if(!require(xlsx))
  install.packages("xlsx")
if(!require(multcomp))
  install.packages("multcomp")

#Read in hw2.dat
#raw_hw2data<-read.table(file=file.choose(), header=T)


#relevel raw_hw2data to have "Yes" as first level of "Breast_Cancer" variable and... 
#"Premenopausal" as first level of "Menopausal_Status" variable... 
#Base R 'read.table' puts levels in alphabetical order,... 
#so "No" and "Postmenopausal" levels would normally come first.
#However, contrasts of Breast_Cancer reveals "No=1" for this case, so don't relevel "Breast_Cancer"
#raw_hw2data$Breast_Cancer<-relevel(raw_hw2data$Breast_Cancer, "Yes", c("Yes", "No"))
raw_hw2data$Menopausal_Status<-relevel(raw_hw2data$Menopausal_Status, "Premenopausal",
                                       c("Premenopausal", "Postmenopausal"))
#check the "contrasts" of each predictor and the outcome!!!

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

####Regression Analysis Q1####
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
###Eventually need to convert below into a loop with ggplot2 graphics###
#get predicted log odds of selected model
logodds<-predict.glm(hw2_forward)
#plot each logodds w/ "points" command
plot(logodds[1:2], main="Predicted Log Odds of Predictors from Selected Model",
     xlab="N/a", ylab = "Ln(Odds)", xaxt='n', col=1,
     type="b",xlim=c(1,3), ylim=c(-1.5, 0), pch=19) #pch)
points(logodds[3:4], col=2, type = "b", pch=19)
points(logodds[5:6], col=3, type="b", pch=19)
points(logodds[7:8], col=4, type="b", pch=19)
points(logodds[9:10], col=5, type ="b", pch=19)
points(logodds[11:12], col=6, type="b", pch=19)
points(logodds[13:14], col=7, type="b", pch=19)
points(logodds[15:16], col=8, type="b", pch=19)

#add legend
parameters<-c("Intercept", "Quartile_biomarker2", "Quartile_biomarker3", "Quartile_biomarker4",
              "Menopausal_StatusPostmenopausal", "Qb2:MSPost", "Qb3:MSPost", "Qb4:MSPost")
colors<-c("1", "2", "3", "4", "5", "6", "7", "8")
legend(x=2.1, y=0.1, legend = parameters, col=colors, pch=19, bty="n", cex=0.9)

#Part D: Goodness of Fit statistic
L_2_sat<--2*logLik(hw2_sat)
L_2_forward<--2*logLik(hw2_forward)
hw2_GoF<-L_2_forward-L_2_sat#hw2_GoF=3.19, df=4(#significant coeffs from L_2_sat-#significant coeffs from L_2_forward)

####Hw Q2####
##Part A-Odds Ratios##
# OR_hw2_forward<-exp(cbind(OR = coef(hw2_forward ), confint(hw2_forward)))
#get data frame of logodds coefficients and their SE's and write to xlsx file
# OR_hw2.df<-data.frame(summary(hw2_forward)$coefficients)
# write.xlsx2(OR_hw2.df, "c:/Users/Rick/Documents/Tulane/MPH/BIOS_7150/Homework/hw2/hw2_OR.xlsx")
# #get covariance matrix for hw2_forward
# hw2_cov<-vcov(hw2_forward)

#Odds Ratios for multiple comparison test of "Menopausal_Status"
K1 <- glht(hw2_forward, mcp(Menopausal_Status = "Tukey"))$linfct
#Odds Ratios for multiple comparison test of "Quartile_Biomarker"
K2 <- glht(hw2_forward, mcp(Quartile_biomarker = "Tukey"))$linfct
#Combine the estimates to get a full pair-wise comparison table
All_OR <- glht(hw2_forward, linfct = rbind(K1, K2))
#Get the Confidence Intervals
All_OR_cint <-confint(All_OR)