##Lecture 19 Script: Conditional logistic regression##
if(!require(tidyverse))
  install.packages("tidyverse")
if(!require(survival))
  install.packages("survival")

#load the livermatch data
livermatch.data<-read_csv(file=file.choose(), col_names = T)

liver.model<-clogit(ct~hep+alc+strata(pair), data=livermatch.data)

#convert columns to class "factor"
# for (i in seq_along(livermatch.data)){
#   livermatch.data[[i]]<-as.factor(livermatch.data[[i]])
# }

#load the stroke data
# stroke.data<-read_csv(file=file.choose(), col_names = T)
#run the stroke model
stroke.model<-clogit(ct~r+e+strata(pair), data=stroke.data)