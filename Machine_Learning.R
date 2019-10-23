library(caret)
library(caTools)
library(MASS)
library(rpart)
library(randomForest)
library(kernlab)
library(gbm)
library(plyr)
library(caret)

getwd()
setwd("G:/Chanel/R-3.4.0/library/project")
Attr.df <-read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv",header = TRUE)
str(Attr.df)
attach(Attr.df)
View(Attr.df)

levels(Attr.df$JobRole) <- c("HC", "HR", "Lab", "Man", "MDir", "RsD", "RsSci", "SlEx", "SlRep")
levels(Attr.df$EducationField) <- c("HR", "LS", "MRK", "MED", "NA", "TD")
Attr.df <- Attr.df[c(-9,-10,-22,-27)]
#split train and test
train<-sample(1:nrow(Attr.df),nrow(Attr.df)*0.7)
test<-Attr.df[-train,]
cl_label<-Attrition[-train]
dim(test)

#defaultSummary: Accuracy,Kappa
#prepare training scheme 
control1 <- trainControl(method="repeatedcv", summaryFunction = defaultSummary,number=10, repeats=3) 
#Boosted Logistics
set.seed(3221) 
fit1.logit<- train(Attrition~., data=Attr.df[train,], method="LogitBoost", trControl=control1) 
#LDA
set.seed(3221) 
fit1.lda <- train(Attrition~., data=Attr.df[train,],method="lda", trControl=control1) 
#KNN 
set.seed(3221) 
fit1.knn <- train(Attrition ~., data=Attr.df[train,], method="knn", trControl=control1)
#CART 
set.seed(3221) 
fit1.cart <- train(Attrition ~., data=Attr.df[train,], method="rpart", trControl=control1)
#Random Forest 
set.seed(3221) 
fit1.rf <- train(Attrition ~., data=Attr.df[train,],method="rf", trControl=control1) 
#SVM 
set.seed(3221) 
fit1.svm <- train(Attrition ~., data=Attr.df[train,], method="svmRadial", trControl=control1) 
#Stochastic Gradient Boosting 
set.seed(3221) 
fit1.gbm <- train(Attrition ~., data=Attr.df[train,],method="gbm", trControl=control1)
#collect resamples 
results1<-resamples(list(Logistics=fit1.logit,LDA=fit1.lda,KNN=fit1.knn,CART=fit1.cart,RF=fit1.rf, SVM=fit1.svm,GBM=fit1.gbm ))
summary(results1)

#twoClassSummary: ROC
library(ROCR)
library(e1071)
library("pROC")
library("pROC")
#prepare training scheme 
control2 <- trainControl(method="repeatedcv", summaryFunction = twoClassSummary,classProbs = TRUE,number=10, repeats=3) 
#Boosted Logistics
set.seed(3221) 
fit2.logit<- train(Attrition~.,data=Attr.df[train,], method="LogitBoost", metric = "ROC", trControl=control2) 
#LDA
set.seed(3221) 
fit2.lda <- train(Attrition~., data=Attr.df[train,], method="lda", metric = "ROC",trControl=control2) 
#KNN 
set.seed(3221) 
fit2.knn <- train(Attrition ~., data=Attr.df[train,], method="knn",  metric = "ROC",trControl=control2)
#CART 
set.seed(3221) 
fit2.cart <- train(Attrition ~., data=Attr.df[train,], method="rpart", metric = "ROC", trControl=control2)
#Random Forest 
set.seed(3221) 
fit2.rf <- train(Attrition ~.,data=Attr.df[train,],method="rf",  metric = "ROC",trControl=control2) 
#SVM 
set.seed(3221) 
fit2.svm <- train(Attrition ~.,data=Attr.df[train,],method="svmRadial",  metric = "ROC",trControl=control2) 
#Stochastic Gradient Boosting 
set.seed(3221) 
fit2.gbm <- train(Attrition ~.,data=Attr.df[train,], method="gbm", metric = "ROC", trControl=control2)
#collect resamples 
results2<-resamples(list(Logistics=fit2.logit,LDA=fit2.lda,KNN=fit2.knn,CART=fit2.cart,RF=fit2.rf, SVM=fit2.svm,GBM=fit2.gbm ))
summary(results2)

#predict
logit2.preds = predict(fit2.logit, newdata=Attr.df[-train,],type = "prob")
lda2.preds = predict(fit2.lda, newdata=Attr.df[-train,] ,type = "prob")
knn2.preds = predict(fit2.knn, newdata=Attr.df[-train,],type = "prob")
cart2.preds = predict(fit2.cart, newdata=Attr.df[-train,],type = "prob")
rf2.preds = predict(fit2.rf,newdata=Attr.df[-train,] ,type = "prob")
svm2.preds = predict(fit2.svm,newdata=Attr.df[-train,] ,type = "prob")
gbm2.preds = predict(fit2.gbm, newdata=Attr.df[-train,] ,type = "prob")

#plot roc
logit2.probs <- prediction(logit2.preds[,2],cl_label) 
perf <- performance(logit2.probs,"tpr","fpr")
plot(perf,col="antiquewhite4",lwd=2)
lda2.probs <- prediction(lda2.preds[,2],cl_label) 
perf <- performance(lda2.probs,"tpr","fpr")
plot(perf,col="orange",add=T,lwd=2)
knn2.probs <- prediction(knn2.preds[,2],cl_label) 
perf <- performance(knn2.probs,"tpr","fpr")
plot(perf,col="chocolate",add=T,lwd=2)
cart2.probs <- prediction(cart2.preds[,2],cl_label) 
perf <- performance(cart2.probs,"tpr","fpr")
plot(perf,col="green",add=T,lwd=2)
rf2.probs <- prediction(rf2.preds[,2],cl_label) 
perf <- performance(rf2.probs,"tpr","fpr")
plot(perf,col="violet",add=T,lwd=2)
svm2.probs <- prediction(svm2.preds[,2],cl_label) 
perf <- performance(svm2.probs,"tpr","fpr")
plot(perf,col="purple",add=T,lwd=2)
gbm2.probs <- prediction(gbm2.preds[,2],cl_label) 
perf <- performance(gbm2.probs,"tpr","fpr")
plot(perf,col="blue",add=T,lwd=2)
legend("right",c("Boosted Logistics","LDA","KNN","CART","Random Forest","SVMRadial","Stochastic Gradient Boosting"),
       col=c("antiquewhite4","orange","chocolate","green","violet","purple","blue"),
       cex=0.8,bty='n',lty=1,lwd=2)
