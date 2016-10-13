library(vcd)
blarg<-data.frame(expand.grid(alcohol=c("No","Yes"),
                              gender=c("male", "female"),
                              ethnic=c("AA", "Hispanic", "White", "Other")
))
blerg<-c(459, 280, 507, 306, 15, 16, 17, 26, 655, 761, 695, 783, 64, 59, 48, 47)
acttsmoke<-cbind(blarg, blerg)
hssmoke<-glm(alcohol~ethnic, family="binomial", data=acttsmoke)
#count=c(459, 15, 655, 64, 507, 17, 695, 48, 
#280, 16, 761, 59, 306, 26, 783, 47)))