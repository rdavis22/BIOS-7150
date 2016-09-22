###BIOS 7150 Homework 1###
#Q1####
data_o=c(55, 68, 95, 82)
spank_o<-matrix(data_o, nrow=2, byrow=T)
data_e<-c(61.5, 61.5, 88.5, 88.5)
spank_e<-matrix(data_e, byrow = T)
library(RVAideMemoire)
L_2=G.test(spank_o)

#Q2####
data_o2=c(56, 22, 12, 60)
spank_o2<-matrix(data_o2, nrow=2, byrow=T)
data_e2<-c(35.36, 42.64, 32.64, 39.36)
spank_e2<-matrix(data_e2, byrow = T)
L_2_prob2=G.test(spank_o2)

#Q3####
#Part a
library(pwr)
tourniquet<-matrix(c(0.1875, 0.045, 0.5625, 0.205), nrow=2, byrow=T)
w <- ES.w2(tourniquet)
tourniquet_pwer<-pwr.chisq.test(w, df=1, power=0.8, sig.level=0.05)
#print(tourniquet_pwer)

#Part b
tourniquet_equln<-matrix(c(0.125, 0.09, 0.375, 0.41), nrow=2, byrow=T)
w_eqln <- ES.w2(tourniquet_equln)
tourniquet_pwer_eqln<-pwr.chisq.test(w_eqln, df=1, power=0.8, sig.level=0.05)
print(tourniquet_pwer_eqln)

#Q4####
library(pROC)
hw1data<-read.table(file="C:/Users/Rick/Documents/Tulane/MPH/BIOS\ 7150/Homework/hw1/Duke\ U\ surgery\ success.dat", header=T, sep="")
attach(hw1data)
numTP<-as.numeric(TP) #make KG and TP predictors numeric
numKG<-as.numeric(KG)

#Generate univariate ROC curves for TP and KG predictors
TProc<-roc(Success~numTP, plot=T, data=hw1data, auc=T, ci=T)
#print(TProc)
KGroc<-roc(Success~numKG, plot=T, data=hw1data, auc=T, ci=T)
#print(KGroc)
vTProc<-var(TProc)
vKGroc<-var(KGroc)
#plotTProc

#Generate multivariate ROC curves for combined TP and KG predictors
multROC <- glm(Success~numTP+numKG, family=binomial(logit))
combroc <-roc(multROC$y , multROC$fitted.values,auc=T, ci=T)
#print(combroc)
vcombroc <- var(combroc)


#Comparing AUC's of ROC curves
compTP_KG<-roc.test(TProc, KGroc)
compTP_combroc<-roc.test(TProc, combroc)
compKG_combroc<-roc.test(KGroc, combroc)

#rocplot_TP<-plot.roc(TProc,col="blue", legacy.axes = T, xlab="1-spec", ylab = "sens", main="ROC curve for TP predictor")
#rocplot_TP<-plot.roc(KGroc,col="blue", legacy.axes = T, xlab="1-spec", ylab = "sens", main="ROC curve for KG predictor")
# rocplot_TP<-plot.roc(combroc,col="blue", legacy.axes = T, xlab="1-spec", ylab = "sens", main="ROC curve for Combined predictor")