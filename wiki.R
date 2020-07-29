n <- names(wiki3)
f <- as.formula(paste("Vandal ~", paste(n[!n %in% "Vandal"], collapse = " + ")))
library(neuralnet)
predict=neuralnet(f,data=wiki3,err.fct = "ce",hidden=3,linear.output = FALSE)
sparseRemoved=as.data.frame(as.matrix(sparseRemoved))
sparseAdded=as.data.frame(as.matrix(sparseAdded))
wiki3=cbind(sparseAdded,sparseRemoved)
wiki3$Loggedin=wiki$Loggedin
wiki3$Minor=wiki$Minor
wiki3$Vandal=wiki$Vandal
n <- names(wiki3)
f <- as.formula(paste("Vandal ~", paste(n[!n %in% "Vandal"], collapse = " + ")))
predict=neuralnet(f,data=wiki3,err.fct = "ce",hidden=3,linear.output = FALSE)
f
colnames(wiki3)=make.names(colnames(wiki3))
n <- names(wiki3)
f <- as.formula(paste("Vandal ~", paste(n[!n %in% "Vandal"], collapse = " + ")))
f
predict=neuralnet(f,data=wiki3,err.fct = "ce",hidden=3,linear.output = FALSE)
table(predict,wiki$Vandal)
table(predict,wiki3$Vandal)
predict$net.result[[1]]
nn=predict$net.result[[1]]
nn=ifelse(nn>0.5,1,0)
table(nn,wiki3$Vandal)
(1055+1769)/(1055+1769+760+292)
#using RF
set.seed(11)
split=sample.split(wiki3$Vandal,SplitRatio=0.7)
library(caTools)
split=sample.split(wiki3$Vandal,SplitRatio=0.7)
trainwiki=subset(wiki3,split==TRUE)
testwiki=subset(wiki3,split==FALSE)
library(randomForest)
modelwiki=randomForest(Vandal~.,data=trainwiki)
predictwiki=predict(modelwiki,newdata=testwiki)
table(testwiki$Vandal,predictwiki)
modelwiki=randomForest(Vandal~.,data=trainwiki,method="class")
predictwiki=predict(modelwiki,newdata=testwiki,type="class")
table(testwiki$Vandal,predictwiki)
predictwiki=predict(modelwiki,newdata=testwiki,type="class")
table(testwiki$Vandal,predictwiki)
modelwiki=randomForest(Vandal~.,data=trainwiki)
wiki3$Vandal=as.factor(wiki3$Vandal)
modelwiki=randomForest(Vandal~.,data=trainwiki)
predictwiki=predict(modelwiki,newdata=testwiki)
table(testwiki$Vandal,predictwiki)
wiki3=as.data.frame(as.matrix(wiki3))
set.seed(11)
wiki3=as.data.frame(as.matrix(wiki3))
split=sample.split(wiki3$Vandal,SplitRatio=0.7)
trainwiki=subset(wiki3,split==TRUE)
testwiki=subset(wiki3,split==FALSE)
modelwiki=randomForest(Vandal~.,data=trainwiki)
predictwiki=predict(modelwiki,newdata=testwiki)
table(testwiki$Vandal,predictwiki)
(295+532)/(295+532+86+250)
predict=neuralnet(f,data=wikitest,err.fct = "ce",hidden=3,linear.output = FALSE)
wiki3=as.data.frame(as.integer(wiki3))
wiki3=as.integer(wiki3)
wiki4=cbind(sparseAdded,sparseRemoved)
wiki4$Minor=wiki$Minor
wiki4$LoggedIn=wiki$Loggedin
wiki4$Vandal=wiki$Vandal
set.seed(11)
split1=sample.split(wiki4$Vandal,SplitRatio=0.7)
trainwiki1=subset(wiki4,split1==TRUE)
testwiki1=subset(wiki4,split1==FALSE)
trainwiki1=subset(wiki4,split==TRUE)
testwiki1=subset(wiki4,split==FALSE)
predict=neuralnet(f,data=testwiki1,err.fct = "ce",hidden=3,linear.output = FALSE)
colnames(testwiki1)=make.names(colnames(testwiki1))
predict=neuralnet(f,data=testwiki1,err.fct = "ce",hidden=3,linear.output = FALSE)
wiki4$Loggedin=wiki$Loggedin
split1=sample.split(wiki4$Vandal,SplitRatio=0.7)
testwiki1=subset(wiki4,split1==FALSE)
predict=neuralnet(f,data=testwiki1,err.fct = "ce",hidden=3,linear.output = FALSE)
f
n <- names(wiki4)
f <- as.formula(paste("Vandal ~", paste(n[!n %in% "Vandal"], collapse = " + ")))
predict=neuralnet(f,data=testwiki1,err.fct = "ce",hidden=3,linear.output = FALSE)
f
predict=neuralnet(f,data=testwiki1,err.fct = "ce",hidden=3,linear.output = FALSE)
colnames(testwiki1)=make.names(colnames(testwiki1))
n <- names(wiki4)
f <- as.formula(paste("Vandal ~", paste(n[!n %in% "Vandal"], collapse = " + ")))
n <- names(testwiki1)
f <- as.formula(paste("Vandal ~", paste(n[!n %in% "Vandal"], collapse = " + ")))
predict=neuralnet(f,data=testwiki1,err.fct = "ce",hidden=3,linear.output = FALSE)
predict$net.result
nn1=predict$net.result
nn1=ifelse(nn1>0.5,1,0)
nn1
nn1=ifelse(nn1>0.5,1,0)
nn=as.integer(nn)
nn1=as.numeric(nn1)
nn1=as.numeric(unlist(nn1)
)
nn1
nn1=ifelse(nn1>0.5,1,0)
table(wikitest1$Vandal,nn1)
table(testwiki1$Vandal,nn1)
(528+341)/(528+341+90+204)
(295+532)/(295+532+86+250)
#using LR
library(glm)
modelLR1=glm(Vandal~.,family="binomial",data=trainwiki1)
predictLR1=predict(modelLR1,newdata=testwiki1)
colnames(testwiki1)=make.names(colnames(testwiki1))
predictLR1=predict(modelLR1,newdata=testwiki1)
View(trainwiki1)
colnames(trainwiki1)=make.names(colnames(trainwiki1))
modelLR1=glm(Vandal~.,family="binomial",data=trainwiki1)
predictLR1=predict(modelLR1,newdata=testwiki1)
predictLR1[1]
predictLR1=predict(modelLR1,newdata=testwiki1,type="response")
predictLR1[1]
predictLR2=predictLR1
predictLR1=ifelse(predictLR1>0.5,1,0)
table(testwiki1$Vandal,predictLR1)
(527+328)/(527+91+217+328)
(295+532+86+250)
#now we use prediction by logistic regression, neural net and random forest to calculate final prediciton.
finalpredict1=nn1+predictRF1+predictLR1
summary(finalpredict1)
finalpredict2=finalpredict1
finalpredict1=ifelse(finalpredict1>1.5,1,0)
table(testwiki1$Vandal,predictfinal1)
finalpredict1[1]
finalpredict1[2]
finalpredict1[3]
finalpredict1[4]
finalpredict1[10:20]
finalpredict1[1:100]
summary(finalpredict1)
nn1[1:10]
predictRF1[1:10]
predictLR1[1:10]
finalpredict1[10]
predictLR1=as.integer(predictLR1)
predictLR1[1:10]
finalpredict1=nn1+predictRF1+predictLR1
finalpredict1[10]
finalpredict1=ifelse(finalpredict1>1.5,1,0)
table(testwiki1$Vandal,predictfinal1)
finalpredict1[10]
sumary(finalpredict1)
summary(finalpredict1)
table(testwiki1$Vandal,predictfinal1)
table(testwiki1$Vandal,finalpredict1)
(531+335)/(531+335+210+87)
table(testwiki1$Vandal,nn1)
(528+341)/(90+204+528+341)


