####BIOS 7150 Hw2####
library(readr)
library(dplyr)
#Read in hw2.dat
raw_hw2data<-read.table(file=file.choose(), header=T)

#relevel raw_hw2data to have "Yes" as first level of "Breast_Cancer" variable and... 
#"Premenopausal" as first level of "Menopausal_Status" variable... 
#Base R 'read.table' puts levels in alphabetical order,... 
#so "No" and "Postmenopausal" levels would normally come first.
raw_hw2data$Breast_Cancer<-relevel(raw_hw2data$Breast_Cancer, "Yes", c("Yes", "No"))
raw_hw2data$Menopausal_Status<-relevel(raw_hw2data$Menopausal_Status, "Premenopausal",
                                       c("Premenopausal", "Postmenopausal"))
attach(raw_hw2data)

#Concatenate by Count variable and index over the other 3 variables to create...
#a 2X4X2 contingency table of "Menopausal_Status"X"Quartile_biomarker"X"Breast_cancer"
hw2data<-tapply(Count, list(Menopausal_Status, Quartile_biomarker, Breast_Cancer), c)
detach(raw_hw2data)

#assign Variable names to newly created contingency table.
names(dimnames(hw2data))<-c("Menopausal_Status", "Quartile_biomarker", "Breast_cancer")
#Create "flattened contingency table"
hw2_table<-ftable(hw2data, row.vars=c("Menopausal_Status", "Quartile_biomarker"), col.vars = "Breast_cancer")
print(hw2_table)

#null <- glm(mg ~ 1,family=binomial)
#sat = glm(mg ~ part+age+trich +part*age + part*trich + trich*age,family=binomial)
#forwards = step(null, scope=list(lower=formula(null), upper=formula(sat)), direction="forward")
#backwards = step(sat)
#nondirect = step(null, list(lower=formula(null), upper=formula(sat)), direction="both",trace=0
