rm(list=ls())
if(!require(tidyverse))
  install.packages("tidyverse")

lec15.data<-read_table(file=file.choose(), col_names = T)
lec15.tibble<-as_tibble(lec15.data)
#change each group in the data set to a factor (need to simplify with dplyr)
lec15.tibble$improv<-factor(improv)
lec15.tibble$treat<-factor(treat)
lec15.tibble$sex<-factor(sex)
lec15.tibble$age<-factor(age)
attach(lec15.tibble)
#fit to fixed main effects model
arthritis <-glm(improv~treat+sex+age, family = "binomial", data=lec15.tibble)
#summary(arthritis)

##Create plot of predicted log odds vs. the age groups
#get the log-odds of "age" levels
age_coeffs<-arthritis$coefficients[3:5]
#get the names of the "age" levels
age_names<-names(arthritis$coefficients[3:5])
#make the x-axis points one group to plot the individual points with connecting...
#lines as seen in "geom_line" statement.
p<-ggplot(group=age_names)+
  geom_point(aes(x=age_names, y=age_coeffs))+
  geom_line(aes(x=age_names, y=age_coeffs), group=1)+
  labs(x='age groups', y='Predicted log odds', title="predicted Log Odds vs age groups")
print(p)

##run glm after assigning contrasts to "age". Apparently, "as.factor" will allow...
##contrasts to be assigned for trend testing, whereas "factor" will not allow such...
##an assignment.

#Each contr.poly statement contains the number of levels of the covariate of interest. "as.factor"
#fac_treat<-as.factor(treat)
# contrasts(fac_treat)<-contr.poly(2)
#fac_sex<-as.factor(sex)
# contrasts(fac_sex)<-contr.poly(2)
fac_age<-as.factor(age)
contrasts(fac_age)<-contr.poly(4)
arthritis2 <-glm(improv~treat+sex+fac_age, family = "binomial", data=lec15.tibble)
summary(arthritis2)