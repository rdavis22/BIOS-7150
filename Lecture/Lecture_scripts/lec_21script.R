####Lec 21 script####
if(!require(tidyverse))
  install.packages("tidyverse")
if(!require(geepack))
  install.packages("geepack")

#load 'geeexample.dat'
geeexample.data<-read_table(file=file.choose(), col_names = T)

#convert "group", "time", and to categorical variables (response can remain "int")
geeexample.data<-mutate_each(geeexample.data, funs(factor), group, time)
attach(geeexample.data)


#run the GEE analysis
g5<-formula(serv~group+time+group*time)
gee1<-geeglm(g5, id=id, family=binomial, corstr = "exchangeable")