set.seed(6001)

train1<-read.csv("trainp3.csv")

names(train1)
dim(train1)
sapply(train1,class)
train1$pclass<-as.factor(train1$pclass)
train1$survfact<-as.factor(train1$survived)

#remove NA roles in embarked
which(is.na(train1$embarked),arr.ind=TRUE)
train1<-train1[-c(62,830),]

#create ticket number field and impute LINE values
splittic<-strsplit(as.character(train1$ticket)," ")
train1$ticknum<-rep(9,dim(train1)[1])
for(i in 1:dim(train1)[1]) {train1$ticknum[i]=as.integer(splittic[[i]][length(splittic[[i]])])}
train1$ticknum[is.na(train1$ticknum)]<-mean(train1$ticknum[!is.na(train1$ticknum)])

#create two ticket phrase field
train1$tick2phrase<-rep(TRUE,dim(train1)[1])
for(i in 1:dim(train1)[1]) {train1$tick2phrase[i]=as.logical(length(splittic[[i]])>1)}

#create class and virility measure
table(train1$title)
train1$cv<-5
train1$cv[train1$title=='Capt']<-0
train1$cv[train1$title=='Col']<-0
train1$cv[train1$title=='Major']<-0
train1$cv[train1$title=='Rev']<-1
train1$cv[train1$title=='Mr']<-2
train1$cv[train1$title=='Sir']<-3
train1$cv[train1$title=='Don']<-3
train1$cv[train1$title=='Mme']<-3
train1$cv[train1$title=='Master']<-3
train1$cv[train1$title=='Miss']<-3
train1$cv[train1$title=='Mlle']<-3
train1$cv[train1$title=='Mrs']<-3
train1$cv[train1$title=='Ms']<-3
train1$cv[train1$title=='Dr']<-2
train1$cv[train1$title=='Jonkheer']<-3
train1$cv[train1$title=='the Countess']<-4
train1$cv[train1$title=='Lady']<-4



#create probe set
library(caret)
set.seed(6001)
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
library(klaR)
library(caTools)
library(glmulti)
library(stats)

#test new models and compare
set.seed(6001)
form20<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname+husband+father+wife+mother+son+daughter")
tunegrid20<-expand.grid(.mtry=15)
#fitcontrol<-trainControl(method="LOOCV")  Use k-fold to maximize speed
fitctrlk<-trainControl(method="repeatedcv",number=10,repeats=3)
ccforest20<-train(form20,data=train3,method="cforest",tuneGrid=tunegrid20,trControl=fitctrlk)
ccforest20$results
varImp(ccforest20)

#OK to set high with a number of noise variables; optimize with lower importance variables removed
set.seed(6001)
form21<-as.formula("survfact~pclass+sex+sibsp+fare+fam+title")
tunegrid21<-expand.grid(.mtry=2:6)
ccforest21<-train(form21,data=train3,method="cforest",tuneGrid=tunegrid21,trControl=fitctrlk)
ccforest21$results
#closer to actual performance

#add cabgr
set.seed(6001)
form22<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname+husband+father+wife+mother+son+daughter+cabgr")
tunegrid22<-expand.grid(.mtry=16)
ccforest22<-train(form22,data=train3,method="cforest",tuneGrid=tunegrid22,trControl=fitctrlk)
ccforest22$results
varImp(ccforest22)
#drop in accuracy

#add cv
set.seed(6001)
form23<-as.formula("survfact~pclass+sex+sibsp+fare+fam+title+cv")
tunegrid23<-expand.grid(.mtry=2:7)
ccforest23<-train(form23,data=train3,method="cforest",tuneGrid=tunegrid23,trControl=fitctrlk)
ccforest23$results
varImp(ccforest23)
#increased to .8123; cv is 100% var imp, mtry=7

#try cv with ccforest20
set.seed(6001)
form24<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname+husband+father+wife+mother+son+daughter+cv")
tunegrid24<-expand.grid(.mtry=10:16)
ccforest24<-train(form24,data=train3,method="cforest",tuneGrid=tunegrid24,trControl=fitctrlk)
ccforest24$results
varImp(ccforest24)
#mtry=11 max=.8314, cv is 100$ var imp 
#Use 23-- 24 may be overfit based on difference in accuracy in validation (compared to actual test results)

#open test
testp1<-read.csv("testp2.csv")
testp1$fam<-as.integer(testp1$fam)
testp1$pclass<-as.factor(testp1$pclass)

#problem with title=Dona and fare=NA
testfaremean<-testp1[-153,]
testp1$fare[153]<-mean(testfaremean$fare[testfaremean$pclass==3])
testp1$title[415]<-"Mrs"

testp1$cv<-5
testp1$cv[testp1$title=='Capt']<-0
testp1$cv[testp1$title=='Col']<-0
testp1$cv[testp1$title=='Major']<-0
testp1$cv[testp1$title=='Rev']<-1
testp1$cv[testp1$title=='Mr']<-2
testp1$cv[testp1$title=='Sir']<-3
testp1$cv[testp1$title=='Don']<-3
testp1$cv[testp1$title=='Mme']<-3
testp1$cv[testp1$title=='Master']<-3
testp1$cv[testp1$title=='Miss']<-3
testp1$cv[testp1$title=='Mlle']<-3
testp1$cv[testp1$title=='Mrs']<-3
testp1$cv[testp1$title=='Ms']<-3
testp1$cv[testp1$title=='Dr']<-2
testp1$cv[testp1$title=='Jonkheer']<-3
testp1$cv[testp1$title=='the Countess']<-4
testp1$cv[testp1$title=='Lady']<-4

survtestp1<-predict(ccforest23,testp1)
dim(testp1)
length(survtestp1)

survtestp1<-data.frame(survtestp1)
write.csv(survtestp1, file = "surv10.csv",row.names=FALSE)
#no increase in accuracy (.78947), try ccforest24

survtestp24<-predict(ccforest24,testp1)
dim(testp1)
length(survtestp24)

survtestp24<-data.frame(survtestp24)
write.csv(survtestp24, file = "surv10a.csv",row.names=FALSE)
#no increase in accuracy (.78947)




