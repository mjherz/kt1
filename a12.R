set.seed(6001)

train1<-read.csv("trainp3.csv")

names(train1)
dim(train1)
sapply(train1,class)
train1$pclass<-as.factor(train1$pclass)
train1$survfact<-as.factor(train1$survived)
train1$fam<-as.integer(train1$fam)
train1$sex<-as.factor(train1$sex)
train1$sibsp<-as.integer(train1$sibsp)
train1$secname<-as.logical(train1$secname)

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

#fix levels for Dona-- in test, not in train
train1title <- factor(train1$title, levels=c(levels(train1$title), 'Dona')) 
train1<-train1[,-18]
train1<-cbind(train1,train1title)
names(train1)[24] <- "title"

#create probe set
#format using first line of train1
library(caret)
set.seed(6001)
trainIndex <- createDataPartition(train1$survived,p = 0.8,list = FALSE,times = 1)
train3<-train1[1,]
probe1<-train1[1,]
train3<-train1[trainIndex,]
probe1<-train1[-trainIndex,]

#also, try random sampling
set.seed(6001)
idx <- sample.int(nrow(train1),size=nrow(train1)*.8) 
train2 <- train1[idx, ] 
probe2 <- train1[-idx, ] 


library(randomForest)

set.seed(6001)
form26<-as.formula("survfact~ticknum+tick2phrase+pclass+sex+sibsp+parch+fare+embarked+fam+title+secname+husband+father+wife+mother+son+daughter")
rf26<-randomForest(form26,data=train3,importance=TRUE)
rf26
importance(rf26)

#filter those below 10%
form27<-as.formula("survfact~ticknum+pclass+sex+sibsp+fare+fam+title+secname")
for(i in 2:8) {
  set.seed(6001)
  rf<-randomForest(form27,data=train3,mtry=i)
  print(rf)
}
#mtry=2--16.85
#mtry=3--17.28
#mtry=4--17.28

set.seed(6001)
rf27<-randomForest(form27,data=train3,mtry=i)

set.seed(6001)
rfcvx27<-train3[,c(2,4,6,9,18,19,22,24)]
rfcv27<-rfcv(rfcvx27,train3$survfact,data=train3,cv.fold=10)
rfcv27$error.cv
# at step 8, corresponds to OOB estimate

set.seed(6001)
fitctrllcv<-trainControl(method="LOOCV")
rf29<-train(form27,data=train3,trControl=fitctrllcv)
rf29
rf29$results
rf29$coefnames



#attempt with randomly sampled data
set.seed(6001)
rf30<-randomForest(form26,data=train2,importance=TRUE)
rf30
importance(rf30)

#filter those below 10%
#same formula as previous
for(i in 2:8) {
  set.seed(6001)
  rf<-randomForest(form27,data=train2,mtry=i)
  print(rf)
}
#mtry=2 16.88
#mtry=3 17.58
#mtry=4 18

set.seed(6001)
rf31<-randomForest(form27,data=train2,mtry=2)

set.seed(6001)
rfcvx31<-train2[,c(2,4,6,9,18,19,22,24)]
rfcv31<-rfcv(rfcvx31,train2$survfact,data=train2,cv.fold=10)
rfcv31$error.cv
# at step 8, corresponds to OOB estimate

set.seed(6001)
fitctrllcv<-trainControl(method="LOOCV")
rf32<-train(form27,data=train2,trControl=fitctrllcv)
rf32
rf32$results
rf32$coefnames

#models to test: rf32, rf31, rf27, rf29

library(class)

survproberf32<-predict(rf32,probe1)
postResample(as.factor(survproberf32),as.factor(probe1$survived))
confusionMatrix(as.factor(survproberf32),as.factor(probe1$survived))$byClass[1:4]
confusionMatrix(as.factor(survproberf32),as.factor(probe1$survived))$table

survproberf31<-predict(rf31,probe1)
postResample(as.factor(survproberf31),as.factor(probe1$survived))
confusionMatrix(as.factor(survproberf31),as.factor(probe1$survived))$byClass[1:4]
confusionMatrix(as.factor(survproberf31),as.factor(probe1$survived))$table

survproberf27<-predict(rf27,probe1)
postResample(as.factor(survproberf27),as.factor(probe1$survived))
confusionMatrix(as.factor(survproberf27),as.factor(probe1$survived))$byClass[1:4]
confusionMatrix(as.factor(survproberf27),as.factor(probe1$survived))$table

survproberf29<-predict(rf29,probe1)
postResample(as.factor(survproberf29),as.factor(probe1$survived))
confusionMatrix(as.factor(survproberf29),as.factor(probe1$survived))$byClass[1:4]
confusionMatrix(as.factor(survproberf29),as.factor(probe1$survived))$table

postResample(as.factor(survproberf32),as.factor(probe1$survived))
postResample(as.factor(survproberf31),as.factor(probe1$survived))
postResample(as.factor(survproberf27),as.factor(probe1$survived))
postResample(as.factor(survproberf29),as.factor(probe1$survived))

#train2 models are not as accurate as train3, but are developed and and trained
#on more random sample-- proceed with more accurate of train2 models-- rf32


#open test
#provide levels for test
testp1<-train1[1,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,24)]
testp1<-rbind(testp1,read.csv("testp2.csv"))
testp1<-testp1[-1,]

testp1$fam<-as.integer(testp1$fam)
testp1$pclass<-as.factor(testp1$pclass)
testp1$sex<-as.factor(testp1$sex)
testp1$sibsp<-as.integer(testp1$sibsp)
testp1$secname<-as.logical(testp1$secname)

#problem with title=Dona and fare=NA
testfaremean<-testp1[-153,]
testp1$fare[153]<-mean(testfaremean$fare[testfaremean$pclass==3])
testp1$title[415]<-"Mrs"

#create ticket number field and impute LINE values
splittic<-strsplit(as.character(testp1$ticket)," ")
testp1$ticknum<-rep(9,dim(testp1)[1])
for(i in 1:dim(testp1)[1]) {testp1$ticknum[i]=as.integer(splittic[[i]][length(splittic[[i]])])}
testp1$ticknum[is.na(testp1$ticknum)]<-mean(testp1$ticknum[!is.na(testp1$ticknum)])


survtestp1<-predict(rf32,testp1)
dim(testp1)
length(survtestp1)

survtestp1<-data.frame(survtestp1)
write.csv(survtestp1, file = "surv12.csv",row.names=FALSE)







