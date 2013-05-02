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
train3<-train1[-c(62,830),]

library(caret)
library(RWeka)
library(gbm)
library(party)
library(glmnet)
library(C50)
library(e1071)
library(mboost)
library(nnet)

fitcontrol<-trainControl(method="repeatedcv",number=10,repeats=10)
fitresults1<-NA
fitresults1<-data.frame(matrix(ncol=4))
form1<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked")

#class-kappa
cripper1<-train(form1,data=train3,method="JRip",metric="Kappa",trControl=fitcontrol)
fitresults1[1,1:2]<-c("jrip","class")
fitresults1[1,3:4]<-cripper1$results[dim(cripper1$results)[1],2:3]

#class-kappa
crf1<-train(form1,data=train3,method="rf",metric="Kappa",trControl=fitcontrol)
fitresults1[2,1:2]<-c("rf","class")
fitresults1[2,3:4]<-crf1$results[dim(crf1$results)[1],2:3]

#class-kappa
cctree1<-train(form1,data=train3,method="ctree",metric="Kappa",trControl=fitcontrol)
fitresults1[3,1:2]<-c("ctree","class")
fitresults1[3,3:4]<-cctree1$results[dim(cctree1$results)[1],2:3]

#class-kappa
ccforest1<-train(form1,data=train3,method="cforest",metric="Kappa",trControl=fitcontrol)
fitresults1[4,1:2]<-c("cforest","class")
fitresults1[4,3:4]<-ccforest1$results[dim(ccforest1$results)[1],2:3]

#class-kappa
cj48tree1<-train(form1,data=train3,method="J48",metric="Kappa",trControl=fitcontrol)
fitresults1[5,1:2]<-c("j48","class")
fitresults1[5,3:4]<-cj48tree1$results[dim(cj48tree1$results)[1],2:3]

#class-kappa
cgbm1<-train(form1,data=train3,method="gbm",metric="Kappa",distribution="bernoulli",trControl=fitcontrol)
fitresults1[6,1:2]<-c("gbm","class")
fitresults1[6,3:4]<-cgbm1$results[dim(cgbm1$results)[1],4:5]

#class-kappa
c50rules1<-train(form1,data=train3,method="C5.0Rules",metric="Kappa",distribution="bernoulli",trControl=fitcontrol)
fitresults1[7,1:2]<-c("c50rules","class")
fitresults1[7,3:4]<-c50rules1$results[dim(c50rules1$results)[1],2:3]

#class-kappa
cPART1<-train(form1,data=train3,method="PART",metric="Kappa",trControl=fitcontrol)
fitresults1[8,1:2]<-c("PART","class")
fitresults1[8,3:4]<-cPART1$results[dim(c50rules1$results)[1],3:4]

#class-kappa
cglmboost1<-train(form1,data=train3,method="glmboost",metric="Kappa",trControl=fitcontrol)
fitresults1[9,1:2]<-c("glmboost","class")
fitresults1[9,3:4]<-cglmboost1$results[dim(cglmboost1$results)[1],3:4]

#class-kappa
cnnet1<-train(form1,data=train3,method="nnet",metric="Kappa",trControl=fitcontrol)
fitresults1[10,1:2]<-c("nnet","class")
fitresults1[10,3:4]<-cnnet1$results[dim(cnnet1$results)[1],3:4]

#class-kappa
cbagFDA1<-train(form1,data=train3,method="bagFDA",metric="Kappa",trControl=fitcontrol)
fitresults1[11,1:2]<-c("bagFDA","class")
fitresults1[11,3:4]<-cbagFDA1$results[dim(cbagFDA1$results)[1],3:4]

#class-kappa
csvm1<-train(form1,data=train3,method="svmPoly",metric="Kappa",trControl=fitcontrol)
fitresults1[12,1:2]<-c("svm","class")
fitresults1[12,3:4]<-csvm1$results[dim(csvm1$results)[1],4:5]

fitresults1

#try BayesTree,evtree

restest3<-data.frame(matrix(nrow=dim(train3)[1]))
restest3$survpred<-as.integer(0)
restest3$agreement<-as.integer(0)
restest3$rip<-as.integer(0)
restest3$rf<-as.integer(0)
restest3$ctree<-as.integer(0)
restest3$cforest<-as.integer(0)
restest3$j48<-as.integer(0)
restest3$gbm<-as.integer(0)
restest3$c50<-as.integer(0)
restest3$PART<-as.integer(0)
restest3$glmboost<-as.integer(0)
restest3$nnet<-as.integer(0)
restest3$bagFDA<-as.integer(0)
restest3$svm<-as.integer(0)

for (i in 1:dim(train3)[1]) {
  j=0
  if(predict(cripper1,train3[i,])==1) {j=j+1;restest3$rip[i]=1}
  if(predict(crf1,train3[i,])==1) {j=j+1;restest3$rf[i]=1}
  if(predict(cctree1,train3[i,])==1) {j=j+1;restest3$ctree[i]=1}
  if(predict(ccforest1,train3[i,])==1) {j=j+1;restest3$cforest[i]=1}
  if(predict(cj48tree1,train3[i,])==1) {j=j+1;restest3$j48[i]=1}
  if(predict(cgbm1,train3[i,])==1) {j=j+1;restest3$gbm[i]=1}
  if(predict(c50rules1,train3[i,])==1) {j=j+1;restest3$c50[i]=1}
  if(predict(cPART1,train3[i,])==1) {j=j+1;restest3$PART[i]=1}
  if(predict(cglmboost1,train3[i,])==1) {j=j+1;restest3$glmboost[i]=1}
  if(predict(cnnet1,train3[i,])==1) {j=j+1;restest3$nnet[i]=1}
  if(predict(cbagFDA1,train3[i,])==1) {j=j+1;restest3$bagFDA[i]=1}
  if(predict(csvm1,train3[i,])==1) {j=j+1;restest3$svm[i]=1}
  restest3$agreement[i]<-j
  if(j>=7) {restest3$survpred[i]<-1}
  print(i)
}

svd1<-svd(scale(restest3[,4:15]))
par(mfrow=c(2,2))
plot(svd1$u[,1],col=(2+train3$survived),pch=19)
plot(svd1$u[,2],col=(2+train3$survived),pch=19)
plot(svd1$u[,3],col=(2+train3$survived),pch=19)
plot(svd1$d^2/sum(svd1$d^2))
svd1$d^2/sum(svd1$d^2)

plot(svd1$v[,1],pch=19)
plot(svd1$v[,2],pch=19)
plot(svd1$v[,3],pch=19)
svd1$v[,1] #4,7,10,12
svd1$v[,2] #2,5,8
svd1$v[,3] #4,12
restest3[1:5,-1]

#svdtest1<-data.frame(as.matrix(restest3[,4:15]) %*% svd1$v)

#redo on training with LOOCV
#limit to 4, 7, 10, 12

fitcontrol<-trainControl(method="LOOCV")
fitresults2<-NA
fitresults2<-data.frame(matrix(ncol=4))

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


test2<-data.frame(as.factor(test1[[1]]))
test2<-cbind(test2,test1[[3]],test1[[5]],test1[[6]],test1[[8]],test1[[10]])
colnames(test2)<-c(names(test1)[[1]],names(test1)[[3]],names(test1)[[5]],names(test1)[[6]],names(test1)[[8]],names(test1)[[10]])
#153 does not have a fare; impute for now
test2$fare[153]<-mean(test1$fare[-c(153)])

restest3a<-data.frame(matrix(nrow=dim(test2)[1]))
restest3a$survpred<-as.integer(0)
restest3a$agreement<-as.integer(0)
restest3a$cforest<-as.integer(0)
restest3a$c50<-as.integer(0)
restest3a$nnet<-as.integer(0)
restest3a$svm<-as.integer(0)

for (i in 1:dim(test2)[1]) {
  j=0
  if(predict(ccforest1,test2[i,])==1) {j=j+1;restest3a$cforest[i]=1}
  if(predict(c50rules1,test2[i,])==1) {j=j+1;restest3a$c50[i]=1}
  if(predict(cnnet1,test2[i,])==1) {j=j+1;restest3a$nnet[i]=1}
  if(predict(csvm1,test2[i,])==1) {j=j+1;restest3a$svm[i]=1}
  restest3a$agreement[i]<-j
  if(j>=3) {restest3a$survpred[i]<-1}
  print(i)
}

#a4 j>=1 submission 4
#a4 j>=3 submission 4a

survpred<-data.frame(restest3a$survpred)
write.csv(survpred, file = "survpred4a.csv",row.names=FALSE)

sum(restest3$survpred==1)/dim(restest3)[1]
sum(train1$survived==1)/dim(train1)[1]

#confusion matrix

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
