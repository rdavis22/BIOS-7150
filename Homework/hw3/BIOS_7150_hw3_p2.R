##Hw3 Problem 2####
#rm(list=ls())
if(!require(tidyverse))
  install.packages("tidyverse")
if(!require(stringr))
  install.packages("stringr")

#read in Mycoplasma Genitalium data
#mg.data<-read_table(file = file.choose(), col_names = TRUE)
#transform all columns except "Count" (needs to be int) into class "factor"
for (i in seq_along(grep("Count", names(mg.data), invert=T))){
  mg.data[[i]]<-as.factor(mg.data[[i]])
}
attach(mg.data)
#get the initial model with age as an ordinal predictor
hw3_p2.model<-glm(mg~num_part+trich+age+num_part*trich,
                  data=mg.data, weights = Count, family=binomial)

#transform age into an interval predictor and see if it fits
contrasts(age)<-contr.poly(3)
cont_age<-as.factor(age)
# attr(,"contrasts")
# .L         .Q
# 1 -7.071068e-01  0.4082483
# 2 -7.850462e-17 -0.8164966
# 3  7.071068e-01  0.4082483
# Levels: 1 2 3

hw3_p2_ord.model<-glm(mg~num_part+trich+cont_age+num_part*trich,
                      data=mg.data, weights = Count, family=binomial)
