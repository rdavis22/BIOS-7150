####BIOS 7150 Hw2####
#rm(list=ls())
if(!require(tidyverse))
  install.packages("tidyverse")
if(!require(Cairo))
  install.packages("Cairo")
if(!require(car))
  install.packages("car")
if(!require(xlsx))
  install.packages("xlsx")
if(!require(multcomp))
  install.packages("multcomp")

#Read in hw2.dat
raw_hw2data<-read.table(file=file.choose(), header=T)


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
#get predicted log odds of selected model
logodds<-predict.glm(hw2_forward)
#get log odds of premenopausal women
lodds_pre<-logodds[seq(1, length(logodds)/2, 2)]
names(lodds_pre)<-c("Quart_bio1", "Quart_bio2",
                    "Quart_bio3", "Quart_bio4")
#get log odds of postmenopausal women
lodds_post<-logodds[seq(length(logodds)/2+1, length(logodds), 2)]
p_df<-data.frame(lodds_pre, lodds_post, names(lodds_pre))
p<-ggplot(p_df)+
  #add postmenopausal log odds points to plot
  geom_point(aes(x=names.lodds_pre., y=lodds_post, colour="postmenopausal"), size=2.5)+
  geom_point(aes(x=names.lodds_pre., y=lodds_pre, colour="premenopausal"), size=2.5)+
  #add a connecting line between points to plot
  geom_line(aes(x=names.lodds_pre., y=lodds_post), group=1)+
  geom_line(aes(x=names.lodds_pre., y=lodds_pre), group=2)+
  #add axis and main title
  labs(title="Predicted log odds for Quartiles in Pre-vs.Post-menopausal women",
       x="Biomarker Quartiles", y="Predicted Log Odds")
print(p)
##Save Plot##
out_flnme<-"BIOS_7150_hw2.png"
path_p<-"c:/Users/Rick/Documents/Tulane/MPH/BIOS\ 7150/Homework/hw2"
ggsave(file=out_flnme, path=path_p)
dev.off()

#Part D: Goodness of Fit statistic
L_2_sat<--2*logLik(hw2_sat)
L_2_forward<--2*logLik(hw2_forward)
hw2_GoF<-L_2_forward-L_2_sat#hw2_GoF=3.19, df=4(#significant coeffs from L_2_sat-#significant coeffs from L_2_forward)

####Hw 2 Q2####
#Odds Ratios for multiple comparison test of "Menopausal_Status"
K1 <- glht(hw2_forward, mcp(Menopausal_Status = "Tukey"))$linfct
#Odds Ratios for multiple comparison test of "Quartile_Biomarker"
K2 <- glht(hw2_forward, mcp(Quartile_biomarker = "Tukey"))$linfct
#Combine the estimates to get a full pair-wise comparison table
All_OR <- glht(hw2_forward, linfct = rbind(K1, K2))
#Get the Confidence Intervals
All_OR_cint <-confint(All_OR)