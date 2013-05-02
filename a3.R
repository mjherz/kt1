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
fitresults1<-NA
fitresults1<-data.frame(matrix(ncol=4))

#class-kappa
cripper1<-train(train2,train1$survfact,method="JRip",trControl=fitcontrol)
fitresults1[1,1:2]<-c("jrip","class")
fitresults1[1,3:4]<-cripper1$results[dim(cripper1$results)[1],2:3]

#class-kappa
form1<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked")
crf1<-train(form1,data=train1,method="rf",trControl=fitcontrol)
fitresults1[2,1:2]<-c("rf","class")
fitresults1[2,3:4]<-crf1$results[dim(crf1$results)[1],2:3]

#class-kappa
#removed embarked due to error
form1<-as.formula("survfact~pclass+sex+sibsp+parch+fare")
cctree1<-train(form1,data=train1,method="ctree",trControl=fitcontrol)
fitresults1[3,1:2]<-c("ctree","class")
fitresults1[3,3:4]<-cctree1$results[dim(cctree1$results)[1],2:3]

#class-kappa
#removed embarked due to error
form1<-as.formula("survfact~pclass+sex+sibsp+parch+fare")
ccforest1<-train(form1,data=train1,method="cforest",trControl=fitcontrol)
fitresults1[4,1:2]<-c("cforest","class")
fitresults1[4,3:4]<-ccforest1$results[dim(ccforest1$results)[1],2:3]

#class-kappa
cj48tree1<-train(train2,train1$survfact,method="J48",trControl=fitcontrol)
fitresults1[5,1:2]<-c("j48","class")
fitresults1[5,3:4]<-cj48tree1$results[dim(cj48tree1$results)[1],2:3]

#class-kappa
cgbm1<-train(train2,train1$survfact,method="gbm",distribution="bernoulli",trControl=fitcontrol)
fitresults1[6,1:2]<-c("gbm","class")
fitresults1[6,3:4]<-cgbm1$results[dim(cgbm1$results)[1],4:5]

#regression-rmse
#form1<-as.formula("survived~pclass+sex+sibsp+parch+fare+embarked")
#cglmnet1<-train(form1,data=train1,method="glmnet",family="binomial",trControl=fitcontrol)
#fitresults1[7,1:2]<-c("glmnet","regression")
#fitresults1[7,3:4]<-cglmnet1$results[dim(cglmnet1$results)[1],3:4]

#class-kappa
form1<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked")
c50rules1<-train(form1,data=train1,method="C5.0Rules")
fitresults1[7,1:2]<-c("c50rules","class")
fitresults1[7,3:4]<-c50rules1$results[dim(c50rules1$results)[1],2:3]

#class-kappa
cPART1<-train(train2,train1$survfact,method="PART",trControl=fitcontrol)
fitresults1[8,1:2]<-c("PART","class")
fitresults1[8,3:4]<-cPART1$results[dim(c50rules1$results)[1],3:4]

test2<-data.frame(as.factor(test1[[1]]))
test2<-cbind(test2,test1[[3]],test1[[5]],test1[[6]],test1[[8]],test1[[10]])
colnames(test2)<-c(names(test1)[[1]],names(test1)[[3]],names(test1)[[5]],names(test1)[[6]],names(test1)[[8]],names(test1)[[10]])
#153 does not have a fare; impute for now
test2$fare[153]<-mean(test1$fare[-c(153)])
restest3<-data.frame(matrix(ncol=2,nrow=dim(test1)[1]))
restest3$survpred<-as.integer(0)
restest3$agreement<-as.integer(0)


#cglmnet fails everything
#csvm and glmstepaic have poor results

for (i in 1:dim(test2)[1]) {
  j=0
  if(predict(cripper1,test2[i,])==1) {j=j+1}
  if(predict(crf1,test2[i,])==1) {j=j+1}
  if(predict(cctree1,test2[i,])==1) {j=j+1}
  if(predict(ccforest1,test2[i,])==1) {j=j+1}
  if(predict(cj48tree1,test2[i,])==1) {j=j+1}
  if(predict(cgbm1,test2[i,])==1) {j=j+1}
  if(predict(c50rules1,test2[i,])==1) {j=j+1}
  if(predict(cPART1,test2[i,])==1) {j=j+1}
  restest3$agreement[i]<-j
  if(j>=2) {restest3$survpred[i]<-1}
}
sum(restest3$survpred==1)/dim(restest3)[1]
sum(train1$survived==1)/dim(train1)[1]

survpred<-data.frame((matrix(nrow=dim(test1)[1])))
survpred[[1]]<-restest3$survpred
write.csv(survpred, file = "survpred3.csv",row.names=FALSE)

#confusion matrix

fitresults1

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
factorial(8)/(factorial(1)*factorial(8-1))*(Prob1^1)*((1-Prob1)^7)
factorial(8)/(factorial(2)*factorial(8-2))*(Prob1^2)*((1-Prob1)^6)
factorial(8)/(factorial(3)*factorial(8-3))*(Prob1^3)*((1-Prob1)^5)
factorial(8)/(factorial(4)*factorial(8-4))*(Prob1^4)*((1-Prob1)^4)
factorial(8)/(factorial(5)*factorial(8-5))*(Prob1^5)*((1-Prob1)^3)
factorial(8)/(factorial(6)*factorial(8-6))*(Prob1^6)*((1-Prob1)^2)
factorial(8)/(factorial(7)*factorial(8-7))*(Prob1^7)*((1-Prob1)^1)
factorial(8)/(factorial(8)*factorial(8-8))*(Prob1^8)*((1-Prob1)^0)