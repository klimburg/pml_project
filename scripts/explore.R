# Explore training set data
require(doSNOW)
registerDoSNOW(makeCluster(5, type="SOCK"))
getDoParWorkers()
getDoParName()
getDoParVersion()

df.train<-read.csv("./data/pml_train.csv",
                   na=c("#DIV/0!","NA"))
df.train<-df.train[,-1]

df.test<-read.csv("./data/pml_test.csv",
                   na=c("#DIV/0!","NA"))
df.test<-df.test[,-1]
library(caret)
allNA<-sapply(df.test,function(x){sum(is.na(x))==length(x)})
#drop all vars that dont appear in test set
df.train.sub<-df.train[,!allNA]

# clean up data
classe.num<-which(names(df.train.sub)=="classe")
summary(df.train.sub)
df.train.sub$cvtd_timestamp<-as.POSIXct(df.train$cvtd_timestamp,
                                    format = "%m/%d/%Y %H:%M")



# 
# #make boxplots
# df.fstat<-data.frame("var.name"=NA,"fstat"=NA)
# row.num<-1
# for(i in 6:(ncol(df.train.sub)-1)){
#     var.name<-names(df.train.sub)[i]
#     lm1<-lm(df.train.sub[,i]~df.train.sub$classe)
#     lm1.summary<-summary(lm1)
#     df.fstat[row.num,1]<-var.name
#     df.fstat[row.num,2]<-lm1.summary$fstatistic[1]
#     row.num<-row.num+1
#     
#     file.name<-paste0("./figures/explore/",
#                       var.name,
#                       ".png")
#     print(file.name)
#     png(filename = file.name,480,480)
# 
#     boxplot(df.train.sub[,i]~df.train.sub$classe,
#             ylab=var.name)
#     dev.off()
# }
# 
# summary(df.train.sub$gyros_forearm_x)
outlier<-which(df.train.sub[,"gyros_forearm_x"] <(-20))
#drop obseveration from data
df.train.sub<-df.train.sub[-outlier,]
summary(df.train.sub)
sapply(df.train.sub,function(x){sum(is.na(x))})

df.fstat<-df.fstat%>%arrange(desc(fstat))
head(df.fstat)

# rpart modesl
set.seed(1234)
model.rpart.cv10x1<-train(classe~.,data=df.train.sub[,-c(1:6)],method="rpart",
                         trControl = trainControl(method = "cv", 
                                                  number = 10,
                                                  repeats = 1))
set.seed(1234)
model.rpart.bs25x1<-train(classe~.,data=df.train.sub[,-c(1:6)],method="rpart",
                         trControl = trainControl(method = "boot", 
                                                  number = 25,
                                                  repeats = 1))


model.rpart.cv10x1
model.rpart.cv10x1$finalModel
pred.train<-predict(model.rpart.cv10x1$finalModel,
                    newdata=df.train.sub[,-c(1:6)],
                    type="class")
fancyRpartPlot(model.rpart.cv10x1$finalModel)
confusionMatrix(pred.train,reference=df.train.sub$classe)

model.rpart.bs25x1
model.rpart.bs25x1$finalModel
pred.train<-predict(model.rpart.bs25x1$finalModel,
                    newdata=df.train.sub[,-c(1:6)],
                    type="class")
fancyRpartPlot(model.rpart.bs25x1$finalModel)
confusionMatrix(pred.train,reference=df.train.sub$classe)


# random forest model



set.seed(1234)
model.rf.cv10x1 <- train(classe~., data=df.train.sub[,-c(1:6)], method="rf",ntree=10,
                         trControl = trainControl(method = "cv", 
                                                  number = 10,
                                                  repeats = 1))

set.seed(1234)
model.rf.bs25x1 <- train(classe~., data=df.train.sub[,-c(1:6)], method="rf",
                           trControl = trainControl(method = "boot", 
                                                    number = 25,
                                                    repeats = 1),
                           ntree=20)

model.rf.cv10x1$modelInfo$parameters
varImp(model.rf.cv10x1)
model.rf.cv10x1$finalModel
pred.train<-predict(model.rf.cv10x1$finalModel,df.train.sub[,-c(1:6)])
confusionMatrix(pred.train,reference=df.train.sub$classe)

model.rf.bs25x1
plot(model.rf.bs25x1)
varImps<-varImp(model.rf.bs25x1)
model.rf.cv10x1$finalModel
pred.train<-predict(model.rf.bs25x1$finalModel,df.train.sub[,-c(1:6)])
confusionMatrix(pred.train,reference=df.train.sub$classe)

pred.test.bs25<-predict(model.rf.bs25x1$finalModel,newdata=df.test[,!allNA])
pred.test.cv10<-predict(model.rf.cv10x1$finalModel,newdata=df.test[,!allNA])

getTree(model.rf.cv10x1$finalModel,k=2,labelVar = T)
