set.seed(6001)

train1<-read.csv("trainp1.csv")

names(train1)
dim(train1)
sapply(train1,class)
train1$pclass<-as.factor(train1$pclass)
train1$survfact<-as.factor(train1$survived)
train1$fam<-rep.int(0,dim(train1)[1])

#get last names
splitnames<-strsplit(as.character(train1$name),",")
firstElement <- function(x){x[1]}
train1$lastnameR<-sapply(splitnames,firstElement)
for(i in 1:dim(train1)[1]) {train1$fam[i]<-table(train1$lastname==train1$lastname[i])[2]}
train1$fam<-as.integer(train1$fam)

#create first digit ticket category
train1$fdticket<-substr(train1$ticket,0,1)
train1$fdticket<-as.factor(train1$fdticket)

#create secondary name category
train1$secname<-regexpr("\\(",train1$name)>0

#develop cabin group
train1$cabgr<-rep("",dim(train1)[1])
train1$cabgr<-substr(train1$cabin,1,1)
train1$cabgr[train1$cabgr==""]<-"U"
train1$cabgr<-as.factor(train1$cabgr)

#missing values
sum(is.na(train1))
which(is.na(train1),arr.ind=TRUE)
#missing age,cabgr
table(train1$cabgr)
sum(train1$cabgr=="")
train1$cabgr[train1$cabgr==""]<-"U"
train1$cabgr<-as.factor(train1$cabgr)
sum(is.na(train1$cabgr))
which(is.na(train1),arr.ind=TRUE)
#missing two values in embarked; insert NA
train1$embarked[train1$embarked==""]<-NA

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
library(klaR)
library(caTools)
library(glmulti)
library(stats)

#test new models and compare
set.seed(6001)
form20<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname+husband+father+wife+mother+son+daughter")
tunegrid2<-expand.grid(.mtry=c(5:15))
fitcontrol<-trainControl(method="LOOCV")
ccforest20<-train(form20,data=train3,method="cforest",tuneGrid=tunegrid2,trControl=fitcontrol)
ccforest20$results

set.seed(6001)
form5<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname")
tunegrid2<-expand.grid(.mtry=c(6:9))
ccforest9<-train(form5,data=train3,method="cforest",tuneGrid=tunegrid2,trControl=fitcontrol)
ccforest9$results

survprobe9<-predict(ccforest9,probe1)
postResample(as.factor(survprobe9),as.factor(probe1$survived))
confusionMatrix(as.factor(survprobe9),as.factor(probe1$survived))$byClass[1:4]
confusionMatrix(as.factor(survprobe9),as.factor(probe1$survived))$table

survprobe20<-predict(ccforest20,probe1)
postResample(as.factor(survprobe20),as.factor(probe1$survived))
confusionMatrix(as.factor(survprobe20),as.factor(probe1$survived))$byClass[1:4]
confusionMatrix(as.factor(survprobe20),as.factor(probe1$survived))$table

testp1<-read.csv("testp1.csv")
testp1$fam<-rep.int(0,dim(testp1)[1])
for(i in 1:dim(testp1)[1]) {testp1$fam[i]<-table(testp1$lastname==testp1$lastname[i])[2]}
testp1$fam<-as.integer(testp1$fam)
testp1$secname<-regexpr("\\(",testp1$name)>0
testp1$pclass<-as.factor(testp1$pclass)

#problem with title=Dona and fare=NA
testfaremean<-testp1[-153,]
testp1$fare[153]<-mean(testfaremean$fare[testfaremean$pclass==3])

survtestp1<-predict(ccforest20,testp1)

survtestp1<-data.frame(survtestp1)
write.csv(survtestp1, file = "surv8.csv",row.names=FALSE)

