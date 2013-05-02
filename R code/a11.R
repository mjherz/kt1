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

#create class and virility measure
table(train1$title)
train1$cv<-as.integer(5)
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
train1$cv<-as.integer(train1$cv)

#fix levels for Dona-- in test, not in train
train1title <- factor(train1$title, levels=c(levels(train1$title), 'Dona')) 
train1<-train1[,-18]
train1<-cbind(train1,train1title)
names(train1)[25] <- "title"

#create probe set
#format using first line of train1
library(caret)
set.seed(6001)
trainIndex <- createDataPartition(train1$survived,p = 0.8,list = FALSE,times = 1)
train3<-train1[1,]
probe1<-train1[1,]
train3<-train1[trainIndex,]
probe1<-train1[-trainIndex,]

library(randomForest)

set.seed(6001)
form23<-as.formula("survfact~pclass+sex+sibsp+fare+fam+title+cv")
rf23<-randomForest(form23,data=train3,importance=TRUE)
rf23
importance(rf23)

set.seed(6001)
form24<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname+husband+father+wife+mother+son+daughter+cv")
rf24<-randomForest(form24,data=train3,importance=TRUE)
rf24
importance(rf24)

set.seed(6001)
form25<-as.formula("survfact~pclass+sex+sibsp+fare+fam+title+cv+secname")
rf25<-randomForest(form25,data=train3,importance=TRUE)
rf25
importance(rf25)

set.seed(6001)
rfcvx23<-train3[,c(2,4,6,9,18,20,25)]
rfcv23<-rfcv(rfcvx23,train3$survfact,data=train3,cv.fold=10)
rfcv23$error.cv

set.seed(6001)
rfcvx24<-train3[,c(2,4,6,7,9,11,12,13,14,15,16,17,18,19,20,25)]
rfcv24<-rfcv(rfcvx24,train3$survfact,data=train3,cv.fold=10)
rfcv24$error.cv

set.seed(6001)
rfcvx25<-train3[,c(2,4,6,9,18,19,20,25)]
rfcv25<-rfcv(rfcvx23,train3$survfact,data=train3,cv.fold=10)
rfcv25$error.cv
#best results in 25

library(class)

survproberf23<-predict(rf23,probe1)
postResample(as.factor(survproberf23),as.factor(probe1$survived))
confusionMatrix(as.factor(survproberf23),as.factor(probe1$survived))$byClass[1:4]
confusionMatrix(as.factor(survproberf23),as.factor(probe1$survived))$table

survproberf24<-predict(rf24,probe1)
postResample(as.factor(survproberf24),as.factor(probe1$survived))
confusionMatrix(as.factor(survproberf24),as.factor(probe1$survived))$byClass[1:4]
confusionMatrix(as.factor(survproberf24),as.factor(probe1$survived))$table

survproberf25<-predict(rf25,probe1)
postResample(as.factor(survproberf25),as.factor(probe1$survived))
confusionMatrix(as.factor(survproberf25),as.factor(probe1$survived))$byClass[1:4]
confusionMatrix(as.factor(survproberf25),as.factor(probe1$survived))$table

postResample(as.factor(survproberf23),as.factor(probe1$survived))
postResample(as.factor(survproberf24),as.factor(probe1$survived))
postResample(as.factor(survproberf25),as.factor(probe1$survived))

library(party)

set.seed(6001)
form24<-as.formula("survfact~pclass+sex+sibsp+parch+fare+embarked+fam+title+secname+husband+father+wife+mother+son+daughter+cv")
cf24<-cforest(form24,data=train3,control=cforest_control(trace=TRUE))
varimp(cf24)

set.seed(6001)
form26<-as.formula("survfact~pclass+sex+fare+fam+title+cv+sibsp")
cf26<-cforest(form26,data=train3,control=cforest_control(trace=TRUE))

survprobecf26<-predict(cf26,probe1,OOB=TRUE)
postResample(as.factor(survprobecf26),as.factor(probe1$survived))
confusionMatrix(as.factor(survprobecf26),as.factor(probe1$survived))$byClass[1:4]
confusionMatrix(as.factor(survprobecf26),as.factor(probe1$survived))$table
#better results in rf25 than with cf models

#open test
#provide levels for test
testp1<-train1[1,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,25)]
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
testp1$cv<-as.integer(testp1$cv)

survtestp1<-predict(rf25,testp1)
dim(testp1)
length(survtestp1)

survtestp1<-data.frame(survtestp1)
write.csv(survtestp1, file = "surv11.csv",row.names=FALSE)







