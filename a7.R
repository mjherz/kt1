set.seed(6001)

train1<-read.csv("train.csv")
test1<-read.csv("test.csv")

names(train1)
dim(train1)
sapply(train1,class)
train1$pclass<-as.factor(train1$pclass)
train1$survfact<-as.factor(train1$survived)
train1$fam<-rep.int(0,dim(train1)[1])

#get last names
splitnames<-strsplit(as.character(train1$name),",")
firstElement <- function(x){x[1]}
train1$lastname<-sapply(splitnames,firstElement)
for(i in 1:dim(train1)[1]) {train1$fam[i]<-table(train1$lastname==train1$lastname[i])[2]}
train1$fam<-as.integer(train1$fam)

#create title category
secElement <- function(x){x[2]}
splitnames2<-sapply(splitnames,secElement)
splitnames3<-strsplit(splitnames2,". ")
train1$title<-sapply(splitnames3,firstElement)
train1$title<-as.factor(train1$title)

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

#For the cabin field I did something similar. "C85" and "C123" maps to "C". 
#Missing "cabin" values I map to "U"(unspecified).

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

fitcontrol<-trainControl(method="LOOCV")
tunegrid2<-expand.grid(.mtry=c(6:11))

#try on train1, limit grid
form5<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname")
ccforest9<-train(form5,data=train3,method="cforest",tuneGrid=tunegrid2,trControl=fitcontrol)
ccforest9$results

form5<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname+age")
ccforest10<-train(form5,data=train3,method="cforest",tuneGrid=tunegrid2,trControl=fitcontrol)
ccforest10$results

form5<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname+age+ticknum")
ccforest11<-train(form5,data=train3,method="cforest",tuneGrid=tunegrid2,trControl=fitcontrol)
ccforest11$results

form5<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname+age+ticknum+tick2phrase")
ccforest12<-train(form5,data=train3,method="cforest",tuneGrid=tunegrid2,trControl=fitcontrol)
ccforest12$results
varImp(ccforest12)

form5<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname+ticknum+tick2phrase")
ccforest13<-train(form5,data=train3,method="cforest",tuneGrid=tunegrid2,trControl=fitcontrol)
ccforest13$results
varImp(ccforest13)

form5<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname+ticknum+tick2phrase+cabgr")
ccforest14<-train(form5,data=train3,method="cforest",tuneGrid=tunegrid2,trControl=fitcontrol)
ccforest14$results
varImp(ccforest14)

#9-title,sex,pclass,fare,secname,fam
#10,11,12,13-same as 9
#14-title, sex, ticknum, secname

tunegrid2<-expand.grid(.mtry=c(4:7))
form5<-as.formula("survfact~pclass+sex+fare+fam+title+secname+ticknum")
ccforest15<-train(form5,data=train3,method="cforest",tuneGrid=tunegrid2,trControl=fitcontrol)
ccforest15$results

form5<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname+ticknum")
tunegrid2<-expand.grid(.mtry=c(4:10))
ccforest16<-train(form5,data=train3,method="cforest",tuneGrid=tunegrid2,trControl=fitcontrol)
ccforest16$results
#slightly better than ccforest9

#test with reproducibility
set.seed(6001)
form5<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname")
tunegrid2<-expand.grid(.mtry=c(6:9))
ccforest9<-train(form5,data=train3,method="cforest",tuneGrid=tunegrid2,trControl=fitcontrol)
ccforest9$results

set.seed(6001)
form5<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname+ticknum")
tunegrid2<-expand.grid(.mtry=c(7:10))
ccforest16<-train(form5,data=train3,method="cforest",tuneGrid=tunegrid2,trControl=fitcontrol)
ccforest16$results
#.002 accuracy improvement.  Try probe1.

survprobe9<-predict(ccforest9,probe1)
postResample(as.factor(survprobe9),as.factor(probe1$survived))
confusionMatrix(as.factor(survprobe9),as.factor(probe1$survived))$byClass[1:4]
confusionMatrix(as.factor(survprobe9),as.factor(probe1$survived))$table

survprobe16<-predict(ccforest16,probe1)
postResample(as.factor(survprobe16),as.factor(probe1$survived))
confusionMatrix(as.factor(survprobe16),as.factor(probe1$survived))$byClass[1:4]
confusionMatrix(as.factor(survprobe16),as.factor(probe1$survived))$table
#worse accuracy; can we use other predictors?

#use unused predictors on records with age
set.seed(6001)
train4<-train3[!(is.na(train3$age)),]
form5<-as.formula("survfact~age+fdticket+cabgr+ticknum+tick2phrase")
tunegrid2<-expand.grid(.mtry=c(3:5))
ccforest17<-train(form5,data=train4,method="cforest",tuneGrid=tunegrid2,trControl=fitcontrol)
ccforest17$results

probe2<-probe1[!(is.na(probe1$age)),]
survprobe17<-predict(ccforest17,probe2)
postResample(as.factor(survprobe17),as.factor(probe2$survived))
confusionMatrix(as.factor(survprobe17),as.factor(probe2$survived))$byClass[1:4]
confusionMatrix(as.factor(survprobe17),as.factor(probe2$survived))$table

#combine ccforest9 with ccforest17; use ccforest17 with age records only
probe3<-probe1[(is.na(probe1$age)),]
survprobe17agena<-predict(ccforest9,probe3)
postResample(as.factor(survprobe17agena),as.factor(probe3$survived))
confusionMatrix(as.factor(survprobe17agena),as.factor(probe3$survived))$byClass[1:4]
confusionMatrix(as.factor(survprobe17agena),as.factor(probe3$survived))$table

probe3$survprobe<-survprobe17agena
probe2$survprobe<-survprobe17
probe4<-rbind(probe3,probe2)

postResample(as.factor(probe4$survprobe),as.factor(probe4$survived))
confusionMatrix(as.factor(probe4$survprobe),as.factor(probe4$survived))$byClass[1:4]
confusionMatrix(as.factor(probe4$survprobe),as.factor(probe4$survived))$table
#worse accuracy

#try the ccforest9 formula with age on records with age
set.seed(6001)
form5<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname+age")
tunegrid2<-expand.grid(.mtry=c(8:10))
ccforest18<-train(form5,data=train4,method="cforest",tuneGrid=tunegrid2,trControl=fitcontrol)
ccforest18$results

survprobe18<-predict(ccforest18,probe2)
postResample(as.factor(survprobe18),as.factor(probe2$survived))
confusionMatrix(as.factor(survprobe18),as.factor(probe2$survived))$byClass[1:4]
confusionMatrix(as.factor(survprobe18),as.factor(probe2$survived))$table

probe2$survprobe<-survprobe18
probe4<-rbind(probe3,probe2)

postResample(as.factor(probe4$survprobe),as.factor(probe4$survived))
confusionMatrix(as.factor(probe4$survprobe),as.factor(probe4$survived))$byClass[1:4]
confusionMatrix(as.factor(probe4$survprobe),as.factor(probe4$survived))$table
#not an improvement in accuracy because too many with age (great results with
#ccforest9 without age)
#age does not improve prediction
















predprobe<-data.frame(survprobe1,probe1$survived)
colnames(predprobe)<-c("pred","ref")
predprobe$cm<-"i"
predprobe$cmnum<-0

for (i in 1:dim(probe1)[1]) {
  if((predprobe$pred[i]=="0") && (predprobe$ref[i]==1)) {predprobe$cm[i]<-"ft";predprobe$cmnum[i]<-1}
  else if ((predprobe$pred[i]=="0") && (predprobe$ref[i]==0)) {predprobe$cm[i]<-"ff";predprobe$cmnum[i]<-2}
  else if ((predprobe$pred[i]=="1") && (predprobe$ref[i]==1)) {predprobe$cm[i]<-"tt";predprobe$cmnum[i]<-3}
  else if((predprobe$pred[i]=="1") && (predprobe$ref[i]==0)) {predprobe$cm[i]<-"tf";predprobe$cmnum[i]<-4}
}

confusionMatrix(as.factor(survprobe1),as.factor(probe1$survived))$table
sum(predprobe$cm=="ff")
sum(predprobe$cm=="ft")

#add fdticket
#form6<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname+fdticket")
#tunegrid2<-expand.grid(.mtry=c(6:10))
#ccforest11<-train(form6,data=train3,method="cforest",tuneGrid=tunegrid2,trControl=fitcontrol)
#ccforest11$results
#ccforest9$results
#fdticket does not help; ccforest9 is better

#survprobe2<-predict(ccforest11,probe1)
#postResample(as.factor(survprobe2),as.factor(probe1$survived))
#confusionMatrix(as.factor(survprobe2),as.factor(probe1$survived))$byClass[1:4]

#confusionMatrix(as.factor(survprobe1),as.factor(probe1$survived))$table
#confusionMatrix(as.factor(survprobe2),as.factor(probe1$survived))$table

#plot(probe1$fam[predprobe$cmnum<=2],col=predprobe$cmnum,pch=19)
#legend(5,5,c("ff","ft","tt","tf"),col=predprobe$cm,pch=19)
library(lattice)
bwplot(probe1$age~predprobe$cm | probe1$sex,notch=TRUE)
#inaccuracies in females when prediction is survive
#inaccuracies in males when prediction is die
bwplot(probe1$age~predprobe$cm | probe1$pclass,notch=TRUE)
bwplot(probe1$age~predprobe$cm | probe1$embarked,notch=TRUE)
bwplot(probe1$age~predprobe$cm | probe1$title,notch=TRUE)
bwplot(probe1$age~predprobe$cm | probe1$secname,notch=TRUE)
#secname (TRUE) + age may distinguish survive prediction errors
bwplot(train3$age~train3$survfact | train3$secname,notch=TRUE)
#secname (FALSE) + age may distinguish survive prediction errors

#segment train1 to create model for records only with age
#trainage4<-train1[!(is.na(train1$age)),]
#trainIndexage <- createDataPartition(trainage4$survived,p = 0.8,list = FALSE,times = 1)
#trainage5<-trainage4[trainIndexage,]
#probe5<-trainage4[-trainIndexage,]

#form7<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname+age")
#tunegrid3<-expand.grid(.mtry=c(6:10))
#ccforest12<-train(form7,data=trainage5,method="cforest",tuneGrid=tunegrid3,trControl=fitcontrol)
#ccforest12$results
#did not improve accuracy

#also sample tickets and find patterns
sample(train1$ticket,10,replace=FALSE)
sample(train1$ticket[train1$survived==1],10,replace=FALSE)
sample(train1$ticket[train1$survived==0],10,replace=FALSE)


#change ticket names to begin with letter vs number






#reformat test
test2<-data.frame(as.factor(test1[[1]]))
test2<-cbind(test2,test1[[3]],test1[[5]],test1[[6]],test1[[8]],test1[[10]])
colnames(test2)<-c(names(test1)[[1]],names(test1)[[3]],names(test1)[[5]],names(test1)[[6]],names(test1)[[8]],names(test1)[[10]])
#153 does not have a fare; impute for now
test2$fare[153]<-mean(test1$fare[-c(153)])

splitnames<-strsplit(as.character(test1$name),",")
test2$lastname<-sapply(splitnames,firstElement)
for(i in 1:dim(test2)[1]) {test2$fam[i]<-table(test2$lastname==test2$lastname[i])[2]}
test2$fam<-as.integer(test2$fam)

splitnames2<-sapply(splitnames,secElement)
splitnames3<-strsplit(splitnames2,". ")
test2$title<-sapply(splitnames3,firstElement)
test2$title<-as.factor(test2$title)
#title has a new category-- Dona, like Mrs.; substitute Mrs
test2$title[test2$title==" Dona"]<-" Mrs"

test2$secname<-regexpr("\\(",test1$name)>0

survpred<-predict(ccforest10,test2)
survpred<-data.frame(survpred)
write.csv(survpred, file = "survpred6comp.csv",row.names=FALSE)

#stop here






for (i in 1:dim(probe1)[1]) {
  j=0
  if(predict(ccforest1,probe1[i,])==1) {j=j+1;resprobe1$cforest1[i]=1}
  if(predict(ccforest2,probe1[i,])==1) {j=j+1;resprobe1$cforest2[i]=1}
  if(predict(glmu1,probe1[i,])==1) {j=j+1;resprobe1$glmu[i]=1}
  resprobe1$agreement[i]<-j
  if(j>=2) {resprobe1$survpred[i]<-1}
  print(i)
}

acc1<-data.frame(matrix(ncol=9,nrow=5))
colnames(acc1)<-c("class","acc","kappa","rmse","rsq","sens","spec","ppv","npv")
acc1[1,1]<-"overall"
acc1[1,2:3]<-postResample(as.factor(resprobe1$survpred),as.factor(probe1$survived))
acc1[1,4:5]<-postResample(resprobe1$survpred,probe1$survived)
acc1[1,6:9]<-confusionMatrix(as.factor(resprobe1$survpred),as.factor(probe1$survived))$byClass[1:4]
acc1[2,1]<-"cforest1"
acc1[2,2:3]<-postResample(as.factor(resprobe1$cforest1),as.factor(probe1$survived))
acc1[2,4:5]<-postResample(resprobe1$cforest1,probe1$survived)
acc1[2,6:9]<-confusionMatrix(as.factor(resprobe1$cforest1),as.factor(probe1$survived))$byClass[1:4]
acc1[3,1]<-"cforest2"
acc1[3,2:3]<-postResample(as.factor(resprobe1$cforest2),as.factor(probe1$survived))
acc1[3,4:5]<-postResample(resprobe1$cforest2,probe1$survived)
acc1[3,6:9]<-confusionMatrix(as.factor(resprobe1$cforest2),as.factor(probe1$survived))$byClass[1:4]
acc1[4,1]<-"glmu"
acc1[4,2:3]<-postResample(as.factor(resprobe1$glmu),as.factor(probe1$survived))
acc1[4,4:5]<-postResample(resprobe1$glmu,probe1$survived)
acc1[4,6:9]<-confusionMatrix(as.factor(resprobe1$glmu),as.factor(probe1$survived))$byClass[1:4]

acc1

#glmu is the most accurate; predict test set
fitcontrol<-trainControl(method="LOOCV")
glmu1<-train(form3,data=train3,method="glm",family="binomial",trControl=fitcontrol)

#reformat test
test2<-data.frame(as.factor(test1[[1]]))
test2<-cbind(test2,test1[[3]],test1[[5]],test1[[6]],test1[[8]],test1[[10]])
colnames(test2)<-c(names(test1)[[1]],names(test1)[[3]],names(test1)[[5]],names(test1)[[6]],names(test1)[[8]],names(test1)[[10]])
#153 does not have a fare; impute for now
test2$fare[153]<-mean(test1$fare[-c(153)])

splitnames<-strsplit(as.character(test1$name),",")
test2$lastname<-sapply(splitnames,firstElement)
for(i in 1:dim(test2)[1]) {test2$fam[i]<-table(test2$lastname==test2$lastname[i])[2]}
test2$fam<-as.integer(test2$fam)

splitnames2<-sapply(splitnames,secElement)
splitnames3<-strsplit(splitnames2,". ")
test2$title<-sapply(splitnames3,firstElement)
test2$title<-as.factor(test2$title)
#title has a new category-- Dona, like Mrs.; substitute Mrs
test2$title[test2$title==" Dona"]<-" Mrs"

test2$secname<-regexpr("\\(",test1$name)>0

survpred<-predict(glmu1,test2)
survpred<-data.frame(survpred)
write.csv(survpred, file = "survpred5glmu.csv",row.names=FALSE)

#assess diversity by Q
#from paper by Kuncheva and Whitaker in Machine learning, 51, 181-207, 2003
#measures of diversity in classifier ensembles and their relationship with the 
#ensemble accuracy
# a d
# c b
#1=cforest1
#2=cforest2
#3=glmu
a12=0
a13=0
a23=0
b12=0
b13=0
b23=0
c12=0
c13=0
c23=0
d12=0
d13=0
d23=0
for (i in 1:dim(probe1)[1]) {
  correct1=FALSE
  correct2=FALSE
  correct3=FALSE
  
  if(predict(ccforest1,probe1[i,])==probe1$survived) {correct1=TRUE}
  if(predict(ccforest2,probe1[i,])==probe1$survived) {correct2=TRUE}
  if(predict(glmu1,probe1[i,])==probe1$survived) {correct3=TRUE}
  
  if(correct1 && correct2) {a12=a12+1}
  if(correct1 && correct3) {a13=a13+1}
  if(correct2 && correct3) {a23=a23+1}
  
  if(!correct1 && !correct2) {b12=b12+1}
  if(!correct1 && !correct3) {b13=b13+1}
  if(!correct2 && !correct3) {b23=b23+1}
  
  if(!correct1 && correct2) {c12=c12+1}
  if(!correct1 && correct3) {c13=c13+1}
  if(!correct2 && correct3) {c23=c23+1}
  
  if(correct1 && !correct2) {d12=d12+1}
  if(correct1 && !correct3) {d13=d13+1}
  if(correct2 && !correct3) {d23=d23+1}
  
  print(i)
}

(a12*b12-c12*d12)/(a12*b12+c12*d12)
(a13*b13-c13*d13)/(a13*b13+c13*d13)
(a23*b23-c23*d23)/(a23*b23+c23*d23)

#retune all on entire train set with LOOCV
fitcontrol<-trainControl(method="LOOCV")

ccforest1<-train(form1,data=train1,method="cforest",trControl=fitcontrol)

ccforest2<-train(form2,data=train1,method="cforest",trControl=fitcontrol)

glmu1<-train(form3,data=train1,method="glm",family="binomial",trControl=fitcontrol)

survpred<-predict(glmu1,test2)
survpred<-data.frame(survpred)
write.csv(survpred, file = "survpred5aglmu.csv",row.names=FALSE)

rm(survpred)
survpred<-(data.frame(matrix(ncol=1,nrow=dim(test1)[1])))
survpred<-rep(0,dim(test1)[1])

for (i in 1:dim(test1)[1]) {
  j=0
  if(predict(ccforest1,test2[i,])==1) {j=j+1}
  if(predict(ccforest2,test2[i,])==1) {j=j+1}
  if(predict(glmu1,test2[i,])==1) {j=j+1}
  if(j>=2) {survpred[i]<-1}
  print(i)
}

write.csv(survpred, file = "survpred5b.csv",row.names=FALSE)

survpred<-predict(ccforest2,test2)
survpred<-data.frame(survpred)
write.csv(survpred, file = "survpred5c.csv",row.names=FALSE)

#add back embarked, like a4probe, keep loocv
form4<-as.formula("survfact~sex+pclass+fare+fam+title+secname")
ccforest4<-train(form4,data=train3,method="cforest",trControl=fitcontrol)

survprobecf2<-predict(ccforest2,probe1)
postResample(as.factor(survprobecf2),as.factor(probe1$survived))
confusionMatrix(as.factor(survprobecf2),as.factor(probe1$survived))$byClass[1:4]

survprobecf4<-predict(ccforest4,probe1)
postResample(as.factor(survprobecf4),as.factor(probe1$survived))
confusionMatrix(as.factor(survprobecf4),as.factor(probe1$survived))$byClass[1:4]