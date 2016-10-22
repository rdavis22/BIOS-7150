####BIOS 7150 Hw2####
#rm(list=ls())
library(readr)
library(dplyr)
library(tibble)
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

#
hw2_null <- glm(Breast_Cancer~ 1, weights=Count, data=raw_hw2data, family=binomial)
hw2_sat = glm(Breast_Cancer ~ Menopausal_Status+Quartile_biomarker+
                Menopausal_Status*Quartile_biomarker, weights = Count,
              data=raw_hw2data, family=binomial)
#forwards = step(null, scope=list(lower=formula(null), upper=formula(sat)), direction="forward")
#backwards = step(sat)
#nondirect = step(null, list(lower=formula(null), upper=formula(sat)), direction="both",trace=0
