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

library(randomForest)
#for first, exclude name, ticket, cabin because of number of factors
#include cabgr (first digit of cabin), and age, but only for complete records
train2<-train1[-which(is.na(train1),arr.ind=TRUE)[,1],]
which(is.na(train2),arr.ind=TRUE)
sum(is.na(train2))
dim(train2)
dim(train1)
form1<-as.formula("survfact~embarked+age+sex+sibsp+parch+pclass+fare+cabgr")
rf1<-randomForest(form1,data=train2,prox=TRUE)
importance(rf1)
train3<-cbind(train2[[2]],train2[[4]],train2[[5]],train2[[6]],train2[[7]],train2[[9]],train2[[11]],train2[[13]])
rfcv1<-rfcv(train3,train2$survfact,cv.fold=10)
rfcv1$error.cv

#exclude cabgr (first digit of cabin), and age because of missing records
rm(train2,train3,form1)
train2<-data.frame(as.factor(train1[[2]]))
train2<-cbind(train2,as.factor(train1[[4]]),as.integer(train1[[6]]),as.integer(train1[[7]]),as.numeric(train1[[9]]),as.factor(train1[[11]]))
colnames(train2)<-c(names(train1)[[2]],names(train1)[[4]],names(train1)[[6]],names(train1)[[7]],names(train1)[[9]],names(train1)[[11]])

library(caret)
library(RWeka)
library(gbm)
library(party)
library(glmnet)
library(C50)
library(e1071)

fitcontrol<-trainControl(method="repeatedcv",number=10,repeats=10)
fitresults1<-data.frame(matrix(ncol=6))

#standardize all errors-- kappa or rmse
cripper1<-train(train2,train1$survfact,method="JRip",trControl=fitcontrol)
fitresults1[1,1]<-"jrip"
fitresults1[1,2:6]<-cripper1$results

form1<-as.formula("survfact~embarked+sex+sibsp+parch+pclass+fare")
crf1<-train(form1,data=train1,method="rf",trControl=fitcontrol)
fitresults1[2,1]<-"rf"
fitresults1[2,2:6]<-crf1$results

cctree1<-train(train2,train1$survfact,method="ctree",trControl=fitcontrol)
fitresults1[3,1]<-"ctree"
fitresults1[3,2:6]<-cctree1$results

#standardize errors
ccforest1<-train(train2,train1$survfact,method="cforest",trControl=fitcontrol)
fitresults1[4,1]<-"cforest"
fitresults1[4,2:6]<-ccforest1$results

#works, but what are the errors
cj48tree1<-train(train2,train1$survfact,method="J48",trControl=fitcontrol)
fitresults1[5,1]<-"j48"
fitresults1[5,2:6]<-cj48tree1$results

#standardize errors
cgbm1<-train(train2,train1$survfact,method="gbm",distribution="bernoulli",trControl=fitcontrol)
fitresults1[6,1]<-"gbm"
fitresults1[6,2:6]<-cgbm1$results

#standardize errors
form1<-as.formula("survived~embarked+sex+sibsp+parch+pclass+fare")
cglmnet1<-train(form1,data=train1,method="glmnet",family="binomial",trControl=fitcontrol)
fitresults1[7,1]<-"glmnet"
fitresults1[7,2:6]<-cglmnet1$results

#works, but not certain of teh type of errors
form1<-as.formula("survfact~embarked+sex+sibsp+parch+pclass+fare")
c50rules1<-train(form1,data=train1,method="C5.0Rules")
fitresults1[8,1]<-"c50rules"
fitresults1[8,2:6]<-c50rules1$results

#standardize errors
cPART1<-train(train2,train1$survfact,method="PART",trControl=fitcontrol)
fitresults1[9,1]<-"part"
fitresults1[9,2:6]<-cPART1$results

form1<-as.formula("survived~embarked+sex+sibsp+parch+pclass+fare")
cglmstepaic1<-train(form1,data=train1,method="glmStepAIC",family="binomial",trControl=fitcontrol)
fitresults1[10,1]<-"glmstepaic"
fitresults1[10,2:6]<-cglmstepaic1$results

csvm1<-train(form1,data=train1,method="svmLinear",family="binomial",trControl=fitcontrol)
fitresults1[11,1]<-"svm"
fitresults1[11,2:6]<-csvm1$results

#can use varImp on some

test2<-data.frame(as.factor(test1[[1]]))
test2<-cbind(test2,as.factor(test1[[3]]),as.integer(test1[[5]]),as.integer(test1[[6]]),as.numeric(test1[[8]]),as.factor(test1[[10]]))
colnames(test2)<-c(names(test1)[[1]],names(test1)[[3]],names(test1)[[5]],names(test1)[[6]],names(test1)[[8]],names(test1)[[10]])
test2$embarked[test2$embarked==""]<-NA
restest3<-data.frame(matrix(ncol=2,nrow=dim(test1)[1]))
restest3$survpred<-as.integer(0)
restest3$agreement<-as.integer(0)
#153 does not have a fare; impute for now
test2$fare[153]<-mean(test1$fare[-c(153)])

for (i in 1:dim(test2)[1]) {
  j=0
  if(predict(cripper1,test2[i,])==1) {j=j+1}
  if(predict(crf1,test2[i,])==1) {j=j+1}
#  if(predict(cctree1,test2[i,])==1) {j=j+1}
#  if(predict(ccforest1,test2[i,])==1) {j=j+1}
  if(predict(cj48tree1,test2[i,])==1) {j=j+1}
  if(predict(cgbm1,test2[i,])==1) {j=j+1}
  if(predict(cglmnet1,test2[i,])==1) {j=j+1}
  if(predict(c50rules1,test2[i,])==1) {j=j+1}
  if(predict(cPART1,test2[i,])==1) {j=j+1}
  if(predict(cglmstepaic1,test2[i,])==1) {j=j+1}
  if(predict(csvm1,test2[i,])==1) {j=j+1}
  restest3$agreement[i]<-j
  if(j>=6) {restest3$survpred[i]<-1}
}
sum(restest3$survpred==1)/dim(restest3)[1]
sum(train1$survived==1)/dim(train1)[1]

survpred<-data.frame((matrix(nrow=dim(test1)[1])))
survpred[[1]]<-restest3$survpred
write.csv(survpred, file = "survpred1.csv",row.names=FALSE)

#confusion matrix
