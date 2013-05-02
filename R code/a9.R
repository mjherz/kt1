set.seed(6001)

train1<-read.csv("trainp2.csv")

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
tunegrid2<-expand.grid(.mtry=15)
fitcontrol<-trainControl(method="LOOCV")
ccforest20<-train(form20,data=train3,method="cforest",tuneGrid=tunegrid2,trControl=fitcontrol)
ccforest20$results
varImp(ccforest20)

#OK to set high with a number of noise variables; optimize with lower importance variables removed
set.seed(6001)
form21<-as.formula("survfact~pclass+sex+sibsp+fare+fam+title")
tunegrid21<-expand.grid(.mtry=2:6)
fitcontrol<-trainControl(method="LOOCV")
ccforest21<-train(form20,data=train3,method="cforest",tuneGrid=tunegrid21,trControl=fitcontrol)
ccforest21$results
#optimal at mtry at 6
#ERROR FOUND HERE
#CORRECT FORM USED
###############################

#try other methods
#tune grid, then do CV
set.seed(6001)
form20<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname+husband+father+wife+mother+son+daughter")
tunegrid3<-expand.grid(.decay=c(.5,.9,1.5,2.5,4),.size=c(seq(1,41,by=5)))
cnnet1<- train(form20,data=train3,method="nnet",maxit = 1000,tuneGrid=tunegrid3)
cnnet1
#size  decay  Accuracy  Kappa  Accuracy SD  Kappa SD
#1     0.5    0.811     0.587  0.0211       0.0429  
#1     0.9    0.809     0.584  0.022        0.0448  
#try c=.3,.4.,5.,6
#size=1,2,3,4,5,6

set.seed(6001)
form20<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname+husband+father+wife+mother+son+daughter")
tunegrid2<-expand.grid(.C=c(seq(.1,10.1,by=.5)))
csvml1<-train(form20,data=train3,method="svmLinear",tuneGrid=tunegrid2)
csvml1
#C     Accuracy  Kappa  Accuracy SD  Kappa SD
#5.6   0.818     0.607  0.0216       0.0469  
#6.1   0.818     0.608  0.0218       0.0474  
#6.6   0.817     0.605  0.0217       0.0474 
#try C=5.7,6.5 by .1

set.seed(6001)
form20<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname+husband+father+wife+mother+son+daughter")
tunegrid2<-expand.grid(.sigma=c(seq(.0005,.0011,by=.00005)),.C=c(seq(45,80,by=1)))
csvmr1<-train(form20,data=train3,method="svmRadial",tuneGrid=tunegrid2)
csvmr1
#31  0.001  0.797     0.569  0.023        0.0451  
#30  0.0101  0.785     0.54   0.0217       0.0442  
#31  0.009  0.786     0.543  0.0235       0.0483  
#34  0.005  0.79      0.552  0.0255       0.0513  
#38  0.001  0.8       0.576  0.0226       0.0445  
#38  0.001  0.8       0.576  0.0226       0.0445  
#  42  0.0011   0.802     0.578  0.0227       0.0448  
#55  9e-04    0.802     0.578  0.0217       0.0431
#75  9e-04    0.803     0.58   0.0217       0.043   

#try with optimized grid
#normally use fitcontrol<-trainControl(method="LOOCV")
#use k-fold to save on time
fitctrlk<-trainControl(method="repeatedcv",number=10,repeats=3)

set.seed(6001)
form20<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname+husband+father+wife+mother+son+daughter")
tunegridl<-expand.grid(.C=c(seq(5.7,6.5,by=.1)))
csvml2<-train(form20,data=train3,method="svmLinear",tuneGrid=tunegridl,trControl=fitctrlk)
csvml2
#The final value used for the model was C = 6.4. GOOD

set.seed(6001)
form20<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname+husband+father+wife+mother+son+daughter")
tunegridnn<-expand.grid(.decay=c(seq(.1,.5,by=.02)),.size=c(1:3))
cnnet2<- train(form20,data=train3,method="nnet",maxit = 1000,tuneGrid=tunegridnn,trControl=fitctrlk)
cnnet2
#The final values used for the model were size = 1 and decay = 0.38.  GOOD

set.seed(6001)
form20<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname+husband+father+wife+mother+son+daughter")
tunegrid2<-expand.grid(.sigma=c(seq(.0007,.0012,by=.00005)),.C=c(seq(70,80,by=1)))
csvmr2<-train(form20,data=train3,method="svmRadial",tuneGrid=tunegrid2,trControl=fitctrlk)
csvmr2
#The final values used for the model were C = 70 and sigma = 0.00075. GOOD

survprobenn2<-predict(cnnet2,probe1)
postResample(as.factor(survprobenn2),as.factor(probe1$survived))
confusionMatrix(as.factor(survprobenn2),as.factor(probe1$survived))$byClass[1:4]
confusionMatrix(as.factor(survprobenn2),as.factor(probe1$survived))$table

survprobe21<-predict(ccforest21,probe1)
postResample(as.factor(survprobe21),as.factor(probe1$survived))
confusionMatrix(as.factor(survprobe21),as.factor(probe1$survived))$byClass[1:4]
confusionMatrix(as.factor(survprobe21),as.factor(probe1$survived))$table

survprobel2<-predict(csvml2,probe1)
postResample(as.factor(survprobel2),as.factor(probe1$survived))
confusionMatrix(as.factor(survprobel2),as.factor(probe1$survived))$byClass[1:4]
confusionMatrix(as.factor(survprobel2),as.factor(probe1$survived))$table

survprober2<-predict(csvmr2,probe1)
postResample(as.factor(survprober2),as.factor(probe1$survived))
confusionMatrix(as.factor(survprober2),as.factor(probe1$survived))$byClass[1:4]
confusionMatrix(as.factor(survprober2),as.factor(probe1$survived))$table

#add nb
set.seed(6001)
form20<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname+husband+father+wife+mother+son+daughter")
#tunegrid2<-expand.grid(.usekernel=TRUE,.fL=c(seq(.00001,.001,by=.00001)))
#tunegrid2<-expand.grid(.usekernel=FALSE,.fL=.1)
#cnb1<-train(form20,data=train3,method="nb",tuneGrid=tunegrid2)
cnb1<-train(form20,data=train3,method="nb")
cnb1

set.seed(6001)
form20<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname+husband+father+wife+mother+son+daughter")
fitctrlloo<-trainControl(method="LOOCV")
cnb2<-train(form20,data=train3,method="nb",trControl=fitctrlloo)
cnb2

set.seed(6001)
form22<-as.formula("survfact~parch+embarked+secname+husband+wife+mother+daughter")
fitctrlloo<-trainControl(method="LOOCV")
cnb3<-train(form22,data=train3,method="nb",trControl=fitctrlloo)
cnb3

survprobenb3<-predict(cnb3,probe1)
postResample(as.factor(survprobenb3),as.factor(probe1$survived))
confusionMatrix(as.factor(survprobenb3),as.factor(probe1$survived))$byClass[1:4]
confusionMatrix(as.factor(survprobenb3),as.factor(probe1$survived))$table

survprobecomb1<-rep.int(0,dim(probe1)[1])
for(i in 1:dim(probe1)[1]) {
  j=0
  if (predict(ccforest21,probe1[i,])==1) {j=j+1}
  if (predict(csvml2,probe1[i,])==1) {j=j+1}
  if (predict(cnnet2,probe1[i,])==1) {j=j+1}
  if (predict(csvmr2,probe1[i,])==1) {j=j+1}
  if (predict(cnb3,probe1[i,])==1) {j=j+1}
  if (j>=3) {survprobecomb1[i]=1}
}

postResample(as.factor(survprobecomb1),as.factor(probe1$survived))
confusionMatrix(as.factor(survprobecomb1),as.factor(probe1$survived))$byClass[1:4]
confusionMatrix(as.factor(survprobecomb1),as.factor(probe1$survived))$table
#slight improvement over last in comparison of accuracy (.8418)

testp1<-read.csv("testp2.csv")
testp1$fam<-as.integer(testp1$fam)
testp1$pclass<-as.factor(testp1$pclass)

#problem with title=Dona and fare=NA
testfaremean<-testp1[-153,]
testp1$fare[153]<-mean(testfaremean$fare[testfaremean$pclass==3])
testp1$title[415]<-"Mrs"

survtestp1<-rep.int(0,dim(testp1)[1])
for(i in 1:dim(testp1)[1]) {
  j=0
  if (predict(ccforest21,testp1[i,])==1) {j=j+1}
  if (predict(csvml2,testp1[i,])==1) {j=j+1}
  if (predict(cnnet2,testp1[i,])==1) {j=j+1}
  if (predict(csvmr2,testp1[i,])==1) {j=j+1}
  if (predict(cnb3,testp1[i,])==1) {j=j+1}
  if (j>=3) {survtestp1[i]=1}
}
dim(testp1)
length(survtestp1)

survtestp1<-data.frame(survtestp1)
write.csv(survtestp1, file = "surv9.csv",row.names=FALSE)

#try again with fewer predictors
survprobecomb2<-rep.int(0,dim(probe1)[1])
for(i in 1:dim(probe1)[1]) {
  j=0
  if (predict(ccforest21,probe1[i,])==1) {j=j+1}
  if (predict(csvml2,probe1[i,])==1) {j=j+1}
  if (predict(cnnet2,probe1[i,])==1) {j=j+1}
  if (j>=2) {survprobecomb2[i]=1}
}

postResample(as.factor(survprobecomb2),as.factor(probe1$survived))
confusionMatrix(as.factor(survprobecomb2),as.factor(probe1$survived))$byClass[1:4]
confusionMatrix(as.factor(survprobecomb2),as.factor(probe1$survived))$table

survtestp2<-rep.int(0,dim(testp1)[1])
for(i in 1:dim(testp1)[1]) {
  j=0
  if (predict(ccforest21,testp1[i,])==1) {j=j+1}
  if (predict(csvml2,testp1[i,])==1) {j=j+1}
  if (predict(cnnet2,testp1[i,])==1) {j=j+1}
  if (j>=2) {survtestp2[i]=1}
}
dim(testp1)
length(survtestp2)

survtestp2<-data.frame(survtestp2)
write.csv(survtestp2, file = "surv9a.csv",row.names=FALSE)

#experiment with cabin
#try grouping by cabin by last name in python
train1$cabgr<-as.factor(substr(train1$cabin,1,1))
table(train1$cabgr)
sum(train1$cabgr=="")
train4<-train1[train1$cabgr!="",]

set.seed(6001)
form22<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname+husband+father+wife+mother+son+daughter+cabgr")
tunegrid22<-expand.grid(.mtry=2:7)
fitctrlk<-trainControl(method="repeatedcv",number=10,repeats=3)
ccforest22<-train(form22,data=train4,method="cforest",tuneGrid=tunegrid22,trControl=fitctrlk)
ccforest22$results

#levels(trainset$outcome) <- list(no="0", yes="1")
#Your output has:
#"At least one of the class levels are not valid R variables names;
#This may cause errors if class probabilities are generated because the variables
#names will be converted to: X0, X1"
#Try changing the factor levels to avoid leading numbers and try again.