# attach(team.batting.00to08)
# forplot <- make.groups(singles = data.frame(value=singles,teamID,yearID,runs),
#   doubles = data.frame(value=doubles,teamID,yearID,runs),
#   triples = data.frame(value=triples,teamID,yearID,runs),
#   homeruns = data.frame(value=homeruns,teamID,yearID,runs),
#   walks = data.frame(value=walks,teamID,yearID,runs),
#   stolenbases = data.frame(value=stolenbases,teamID,yearID,runs),
#   caughtstealing = data.frame(value=caughtstealing,teamID,yearID,runs),
#   hitbypitch = data.frame(value=hitbypitch,teamID,yearID,runs),
#   sacrificeflies = data.frame(value=sacrificeflies,teamID,yearID,runs),
#   );
# detach(team.batting.00to08)
rm(list=ls())
library(vcd)
acttsmoke<-data.frame
#alcohol<-c("No","Yes")
#gender<-c("male", "female")
alcohol=c("No","Yes"),
gender=c("male", "female"),
ethnic<-c("AA", "Hispanic", "White", "Other"),
actcount<-c(459, 280, 507, 306, 15, 16, 17, 26, 655, 761, 695, 783, 64, 59, 48, 47)
#acttsmoke<-cbind(alcohol, actcount)
attach(acttsmoke)
hssmoke<-glm(alcohol~ethnic, family="binomial", data=acttsmoke)
                              #count=c(459, 15, 655, 64, 507, 17, 695, 48, 
                                      #280, 16, 761, 59, 306, 26, 783, 47)))
v<-vcov(hssmoke)
logLik(hssmoke)