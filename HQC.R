library(data.table)
library(caret)
library(randomForest)
# read the dataset into R
train<-fread("train.csv",drop=c("QuoteNumber","Original_Quote_Date"),na.strings = c("NA","NAN",""," "))
train<-as.data.frame(train)
hist(train$QuoteConversion_Flag)
train$QuoteConversion_Flag<-as.factor(train$QuoteConversion_Flag)
nzv<-nearZeroVar(train)
train<-train[,-nzv]
#isNum<-NULL
for(i in seq(2,dim(train)[2]))
{
  #if(class(train[,i]) %in% c("numeric","integer"))
  #{isNum<-c(isNum,i) }
  #else 
  if(!(class(train[,i]) %in% c("numeric","integer")))
    { train[,i]<-as.integer(as.factor(train[,i]))}
  
}


#split the dataset into positive and negative ones, sampling respectively to make a balanced dataset
set.seed(1765)
posSeq<-sample(1:dim(train[train$QuoteConversion_Flag==1,])[1],size = 40000)
negSeq<-sample(1:dim(train[train$QuoteConversion_Flag==1,])[1],size = 40000)
pos<-train[train$QuoteConversion_Flag==1,]
neg<-train[train$QuoteConversion_Flag==0,]
training<-rbind.data.frame(pos[posSeq,],neg[negSeq,])
testing<-rbind.data.frame(pos[-posSeq,],neg[-negSeq,])
rm(posSeq,negSeq,pos,neg,train,i)
preObj<-preProcess(training[,-1],method = c("knnImpute","scale","center"))
training[,-1]<-predict(preObj,training[,-1])
model<-randomForest(y=training[,1],x=training[,-1],ntree = 100,importance=TRUE)
testing[,-1]<-predict(preObj,testing[,-1])
pred <- predict(model,newdata=testing[,-1],type="class")
confusionMatrix(pred,testing$QuoteConversion_Flag)
test<-fread("test.csv",drop=c("QuoteNumber","Original_Quote_Date"),na.strings = c("NA","NAN",""," "))
test<-as.data.frame(test)
test<-test[,-(nzv-1)]
for(i in seq(1,dim(test)[2]))
{
  if(!class(test[,i]) %in% c("numeric","integer"))
  { test[,i]<-as.integer(factor(test[,i]))}
  
}
test<-predict(preObj,test)
QuoteConversion_Flag<-predict(model,newdata=test,type="class")
quoNum<-fread("test.csv",select="QuoteNumber")
prediction<-data.frame(quoNum,QuoteConversion_Flag)
write.csv(prediction, "pred.csv",row.names=FALSE)