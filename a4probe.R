set.seed(6001)

train1<-read.csv("train.csv")
test1<-read.csv("test.csv")
gcm<-read.csv("genderclassmodel.csv")
gm<-read.csv("gendermodel.csv")

names(train1)
dim(train1)
sapply(train1,class)
train1$pclass<-as.factor(train1$pclass)
train1$survfact<-as.factor(train1$survived)

#develop cabin group
train1$cabgr<-as.factor(substr(train1$cabin,1,1))
table(train1$cabgr)
sum(train1$cabgr=="")

#missing values
sum(is.na(train1))
which(is.na(train1),arr.ind=TRUE)
#missing age,cabgr
train1$cabgr[train1$cabgr==""]<-NA
train1$cabgr<-as.factor(train1$cabgr)
train1$survdoub<-as.double(train1$survived)
sum(is.na(train1$cabgr))
which(is.na(train1),arr.ind=TRUE)
#missing two values in embarked; insert NA
train1$embarked[train1$embarked==""]<-NA

#exclude cabgr (first digit of cabin), and age because of missing records
#train2<-data.frame(as.factor(train1[[2]]))
#train2<-cbind(train2,as.factor(train1[[4]]),as.integer(train1[[6]]),as.integer(train1[[7]]),as.numeric(train1[[9]]),as.factor(train1[[11]]))
#colnames(train2)<-c(names(train1)[[2]],names(train1)[[4]],names(train1)[[6]],names(train1)[[7]],names(train1)[[9]],names(train1)[[11]])

#remove NA roles in embarked
which(is.na(train1$embarked),arr.ind=TRUE)
train1<-train1[-c(62,830),]

#create probe set
library(caret)
trainIndex <- createDataPartition(train1$survived,p = 0.8,list = FALSE,times = 1)
train3<-train1[trainIndex,]
probe1<-train1[-trainIndex,]

library(RWeka)
library(gbm)
library(party)
library(glmnet)
library(C50)
library(e1071)
library(mboost)
library(nnet)
library(kernlab)

#limit to 4, 7, 10, 12 from a4

fitcontrol<-trainControl(method="LOOCV")
fitresults2<-NA
fitresults2<-data.frame(matrix(ncol=4))
form1<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked")

ccforest1<-train(form1,data=train3,method="cforest",metric="Kappa",trControl=fitcontrol)
fitresults2[1,1:2]<-c("cforest","class")
fitresults2[1,3:4]<-ccforest1$results[dim(ccforest1$results)[1],2:3]

c50rules1<-train(form1,data=train3,method="C5.0Rules",metric="Kappa",distribution="bernoulli",trControl=fitcontrol)
fitresults2[2,1:2]<-c("c50rules","class")
fitresults2[2,3:4]<-c50rules1$results[dim(c50rules1$results)[1],2:3]

cnnet1<-train(form1,data=train3,method="nnet",metric="Kappa",trControl=fitcontrol)
fitresults2[3,1:2]<-c("nnet","class")
fitresults2[3,3:4]<-cnnet1$results[dim(cnnet1$results)[1],3:4]

csvm1<-train(form1,data=train3,method="svmPoly",metric="Kappa",trControl=fitcontrol)
fitresults2[4,1:2]<-c("svm","class")
fitresults2[4,3:4]<-csvm1$results[dim(csvm1$results)[1],4:5]

fitresults2

resprobe1<-data.frame(matrix(nrow=dim(probe1)[1]))
resprobe1$survpred<-as.integer(0)
resprobe1$agreement<-as.integer(0)
resprobe1$cforest<-as.integer(0)
resprobe1$c50<-as.integer(0)
resprobe1$nnet<-as.integer(0)
resprobe1$svm<-as.integer(0)

#calculate accuracy for probe set with prediction and confusion matrix functions
for (i in 1:dim(probe1)[1]) {
  j=0
  if(predict(ccforest1,probe1[i,])==1) {j=j+1;resprobe1$cforest[i]=1}
  if(predict(c50rules1,probe1[i,])==1) {j=j+1;resprobe1$c50[i]=1}
  if(predict(cnnet1,probe1[i,])==1) {j=j+1;resprobe1$nnet[i]=1}
  if(predict(csvm1,probe1[i,])==1) {j=j+1;resprobe1$svm[i]=1}
  resprobe1$agreement[i]<-j
  if(j>=3) {resprobe1$survpred[i]<-1}
  print(i)
}

acc1<-data.frame(matrix(ncol=9,nrow=5))
colnames(acc1)<-c("class","acc","kappa","rmse","rsq","sens","spec","ppv","npv")
acc1[1,1]<-"overall"
acc1[1,2:3]<-postResample(as.factor(resprobe1$survpred),as.factor(probe1$survived))
acc1[1,4:5]<-postResample(resprobe1$survpred,probe1$survived)
acc1[1,6:9]<-confusionMatrix(as.factor(resprobe1$survpred),as.factor(probe1$survived))$byClass[1:4]
acc1[2,1]<-"cforest"
acc1[2,2:3]<-postResample(as.factor(resprobe1$cforest),as.factor(probe1$survived))
acc1[2,4:5]<-postResample(resprobe1$cforest,probe1$survived)
acc1[2,6:9]<-confusionMatrix(as.factor(resprobe1$cforest),as.factor(probe1$survived))$byClass[1:4]
acc1[3,1]<-"c50"
acc1[3,2:3]<-postResample(as.factor(resprobe1$c50),as.factor(probe1$survived))
acc1[3,4:5]<-postResample(resprobe1$c50,probe1$survived)
acc1[3,6:9]<-confusionMatrix(as.factor(resprobe1$c50),as.factor(probe1$survived))$byClass[1:4]
acc1[4,1]<-"nnet"
acc1[4,2:3]<-postResample(as.factor(resprobe1$nnet),as.factor(probe1$survived))
acc1[4,4:5]<-postResample(resprobe1$nnet,probe1$survived)
acc1[4,6:9]<-confusionMatrix(as.factor(resprobe1$nnet),as.factor(probe1$survived))$byClass[1:4]
acc1[5,1]<-"svm"
acc1[5,2:3]<-postResample(as.factor(resprobe1$svm),as.factor(probe1$survived))
acc1[5,4:5]<-postResample(resprobe1$svm,probe1$survived)
acc1[5,6:9]<-confusionMatrix(as.factor(resprobe1$svm),as.factor(probe1$survived))$byClass[1:4]
#cforest has greatest accuracy, greater than overall on probe set

#retune cforest on entire train set
fitcontrol<-trainControl(method="LOOCV")
form1<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked")
ccforest2<-train(form1,data=train1,method="cforest",metric="Kappa",trControl=fitcontrol)

test2<-data.frame(as.factor(test1[[1]]))
test2<-cbind(test2,test1[[3]],test1[[5]],test1[[6]],test1[[8]],test1[[10]])
colnames(test2)<-c(names(test1)[[1]],names(test1)[[3]],names(test1)[[5]],names(test1)[[6]],names(test1)[[8]],names(test1)[[10]])
#153 does not have a fare; impute for now
test2$fare[153]<-mean(test1$fare[-c(153)])

survpred<-predict(ccforest2,test2)
survpred<-data.frame(survpred)
write.csv(survpred, file = "survpred4probe.csv",row.names=FALSE)


#library(randomForest)
#for first, exclude name, ticket, cabin because of number of factors
#include cabgr (first digit of cabin), and age, but only for complete records
#form1<-as.formula("survfact~embarked+age+sex+sibsp+parch+pclass+fare+cabgr")
#rf1<-randomForest(form1,data=train2,prox=TRUE)
#importance(rf1)
#train3<-cbind(train2[[2]],train2[[4]],train2[[5]],train2[[6]],train2[[7]],train2[[9]],train2[[11]],train2[[13]])
#rfcv1<-rfcv(train3,train2$survfact,cv.fold=10)
#rfcv1$error.cv

Prob1=.79
factorial(4)/(factorial(1)*factorial(4-1))*(Prob1^1)*((1-Prob1)^3)
factorial(4)/(factorial(2)*factorial(4-2))*(Prob1^2)*((1-Prob1)^2)
factorial(4)/(factorial(3)*factorial(4-3))*(Prob1^3)*((1-Prob1)^1)
factorial(4)/(factorial(4)*factorial(4-4))*(Prob1^4)*((1-Prob1)^0)
