library(ggplot2)
library(grid)
library(gridExtra)
library(plyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(gbm)
library(pROC)
library(ROCR)
library(scales)
 
getwd()
setwd("G:/Chanel/R-3.4.0/library/project")
Attr.df<-read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv",header=TRUE)
str(Attr.df)

summary(Attr.df)

cbPalette <- c("#BF3EFF", "#9AFF9A")

g1<-ggplot(Attr.df,aes(x=Age,fill=Attrition))+ 
  geom_density(alpha=0.7,colour="white")+scale_fill_manual(values=cbPalette)

g2<-ggplot(Attr.df,aes(x= Gender,fill=Attrition))+ 
  geom_bar(position="fill")+scale_fill_manual(values=cbPalette)+
  labs(y="Percentage")+scale_y_continuous(labels=percent) 

g3<-ggplot(Attr.df,aes(x=Education,fill=Attrition))+ 
  geom_bar(position="fill")+scale_fill_manual(values=cbPalette)+
  labs(y="Percentage") + scale_y_continuous(labels=percent) 

g4<-ggplot(Attr.df, aes(x= JobLevel,fill = Attrition))+ 
  geom_bar(position="fill")+scale_fill_manual(values=cbPalette)+
  labs(y="Percentage")+scale_y_continuous(labels=percent)
grid.arrange(g1,g2,g3,g4,ncol=2,nrow=2)


g5<-ggplot(Attr.df,aes(x=NumCompaniesWorked,fill=Attrition))+ 
  geom_density(alpha = 0.7,colour="white")+scale_fill_manual(values=cbPalette)

g6<-ggplot(Attr.df,aes(x=TotalWorkingYears,fill=Attrition))+ 
  geom_density(alpha=0.7,colour="white")+scale_fill_manual(values=cbPalette)

g7<-ggplot(Attr.df,aes(x=YearsAtCompany,fill=Attrition))+ 
  geom_density(alpha=0.7,colour="white")+scale_fill_manual(values=cbPalette)

g8<-ggplot(Attr.df,aes(x=Department,fill=Attrition))+ 
  geom_bar(position="fill")+scale_fill_manual(values=cbPalette)+
  labs(y="Percentage")+scale_y_continuous(labels=percent)
grid.arrange(g5,g6,g7,g8,ncol=2,nrow=2)


g9<-ggplot(Attr.df,aes(x=MonthlyIncome,fill=Attrition))+ 
  geom_density(alpha=0.7,colour="white")+scale_fill_manual(values=cbPalette)

g10<-ggplot(Attr.df,aes(x=JobInvolvement,fill=Attrition))+ 
  geom_bar(position="fill")+scale_fill_manual(values=cbPalette)+
  labs(y="Percentage")+scale_y_continuous(labels=percent)
grid.arrange(g9,g10,ncol=2)
cbPalette1 <- c("#AB82FF", "#CAFF70","#FFEC8B","#FFC1C1")

ggplot(Attr.df,aes(x=JobInvolvement,y=MonthlyIncome,group=JobInvolvement))+ 
  geom_boxplot(aes(fill=factor(..x..)),alpha=0.7)+scale_fill_manual(values=cbPalette1)+ 
  theme(legend.position="none",plot.title=element_text(hjust=0.5))+
  facet_grid(~Attrition)+ggtitle("Attrition")


g11<-ggplot(Attr.df,aes(x=PercentSalaryHike,fill=Attrition))+ 
  geom_density(alpha=0.7,colour="white")+scale_fill_manual(values=cbPalette)

g12<-ggplot(Attr.df,aes(x=YearsSinceLastPromotion,fill=Attrition))+ 
  geom_density(alpha=0.7,colour="white")+scale_fill_manual(values=cbPalette)

g13<-ggplot(Attr.df,aes(x=TrainingTimesLastYear,fill=Attrition))+ 
  geom_bar(position="fill")+scale_fill_manual(values=cbPalette)+
  labs(y="Percentage")+scale_y_continuous(labels=percent)

g14<-ggplot(Attr.df,aes(x=StockOptionLevel,fill=Attrition))+ 
  geom_bar(position="fill")+labs(y="Percentage")+scale_y_continuous(labels=percent)+scale_fill_manual(values=cbPalette)
grid.arrange(g11,g12,g13,g14,ncol=2)


g15<-ggplot(Attr.df,aes(x=JobSatisfaction,fill=Attrition))+ 
  geom_bar(position="fill")+scale_fill_manual(values=cbPalette)+
  labs(y="Percentage")+scale_y_continuous(labels=percent)

g16<-ggplot(Attr.df,aes(x=RelationshipSatisfaction,fill=Attrition))+ 
  geom_bar(position="fill")+scale_fill_manual(values=cbPalette)+
  labs(y="Percentage")+scale_y_continuous(labels=percent) 

g17<-ggplot(Attr.df,aes(x=EnvironmentSatisfaction,fill=Attrition))+ 
  geom_bar(position="fill")+scale_fill_manual(values=cbPalette)+
  labs(y="Percentage")+scale_y_continuous(labels=percent) 
grid.arrange(g15,g16,g17,ncol=3)


g18<-ggplot(Attr.df,aes(x=DistanceFromHome,fill=Attrition))+
  geom_density(alpha=0.7,colour="white")+scale_fill_manual(values=cbPalette)

g19<-ggplot(Attr.df,aes(x=OverTime,fill=Attrition))+ 
  geom_bar(position="fill")+scale_fill_manual(values=cbPalette)+
  labs(y="Percentage") + scale_y_continuous(labels=percent) 

g20<-ggplot(Attr.df,aes(x= BusinessTravel,fill=Attrition))+ 
  geom_bar(position="fill")+scale_fill_manual(values=cbPalette)+
  labs(y="Percentage")+scale_y_continuous(labels=percent) 

g21<-ggplot(Attr.df,aes(x= WorkLifeBalance,fill=Attrition))+ 
  geom_bar(position="fill")+scale_fill_manual(values=cbPalette)+
  labs(y="Percentage")+scale_y_continuous(labels=percent) 
grid.arrange(g18,g19,g20,g21,ncol=2)


attach(Attr.df)
set.seed(3221)

levels(Attr.df$JobRole) <- c("HC", "HR", "Lab", "Man", "MDir", "RsD", "RsSci", "SlEx", "SlRep")
levels(Attr.df$EducationField) <- c("HR", "LS", "MRK", "MED", "NA", "TD")
Attr.df <- Attr.df[c(-9,-10,-22,-27)]

train<-sample(1:nrow(Attr.df),nrow(Attr.df)*0.7)
test<-Attr.df[-train,]
cl_label<-Attrition[-train]
dim(test)
length(train)


#logistics
glm.fit=glm(Attrition~.,data=Attr.df,family=binomial,subset=train)
summary(glm.fit)

summary(glm.fit)$coef[,4]
glm.probs=predict(glm.fit,newdata=test,type="response")
summary(glm.probs)
#roc
library(ROCR)
logit.pred <- prediction(glm.probs,cl_label) 
perf <- performance(logit.pred,"tpr","fpr")
plot(perf,col="violet",lwd=2)

contrasts(Attr.df$Attrition)
glm.pred=rep("No",441)
glm.pred[glm.probs>.5]="Yes"
glm.pred<-factor(glm.pred)
length(glm.pred)
length(test$Attrition)
t=table(glm.pred,test$Attrition)
mean(glm.pred==test$Attrition)


glm.logit=glm(Attrition~.,data=Attr.df[train,],family=binomial)
summary(glm.logit)
p=predict(glm.logit,newdata=test)
p=exp(p)/(1+exp(p))
TPR=rep(0,1000)
FPR=rep(0,1000)
for(i in 1:1000){
  p0=i/1000;
  Attrition.true=test$Attrition
  Attrition.true<-ifelse(Attrition.true== "No", 1, 0)
  Attrition.pred=ifelse(p>p0,0,1)
  TPR[i]=sum(Attrition.pred*Attrition.true)/sum(Attrition.true)
  FPR[i]=sum(Attrition.pred*(1-Attrition.true))/sum(1-Attrition.true)}
plot(FPR,TPR,type="l",col=2)
points(c(0,1),c(0,1),type="l",lty=2)


#LDA
library(MASS)
lda.fit=lda(Attrition~.,data=Attr.df,subset=train,cv=T)
lda.fit
summary(lda.fit)
lda.pred=predict(lda.fit,newdata=test)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class,test$Attrition)
mean(lda.class==test$Attrition)
#roc
library(ROCR)
#choose the posterior probability column  
pred <- prediction(lda.pred$posterior[,2], cl_label) 
perf <- performance(pred,"tpr","fpr")
plot(perf,col="chocolate",lwd=2)


#KNN
attach(Attr.df)
library(caret)
library(kknn)
set.seed(3221)
train<-sample(1:nrow(Attr.df),nrow(Attr.df)*0.7)
knn.train<- Attr.df[train,]
knn.test<- Attr.df[-train,]
cl_label<-Attrition[-train]
fit.kknn <- kknn(Attrition~., knn.train,knn.test, distance = 1, kernel = "triangular")  
summary(fit.kknn)
fit<-fitted(fit.kknn)
table(knn.test$Attrition, fit) 
#roc
knn.pred <- prediction(fit.kknn$prob[,2],cl_label) 
perf <- performance(knn.pred,"tpr","fpr")
plot(perf,col="green",lwd=2)


#Decision Tree
set.seed(3221)
n <- nrow(Attr.df)
rnd <- sample(n,n*0.7)
train <- Attr.df[rnd,]
test <- Attr.df[-rnd,]

dtree<-rpart(Attrition~.,data=train)
preds<-predict(dtree,test,type='class')
trprobs <- predict(dtree,test,type='prob')
table(preds,test$Attrition)
rocv<-roc(as.numeric(test$Attrition),as.numeric(preds))
rocv$auc
prop.table(table(test$Attrition,preds,dnn = c('Actual','Predicted')),1)
dtreepr<-prune(dtree,cp=0.01666667)
predspr<-predict(dtreepr,test,type='class')
rocvpr<-roc(as.numeric(test$Attrition),as.numeric(predspr))
rocvpr$auc
rpart.plot(dtreepr,type=4,extra=104,tweak=0.9,fallen.leaves=F,cex=0.7)
#roc
tree.pred <- prediction(trprobs[,2],cl_label) 
perf <- performance(tree.pred,"tpr","fpr")
plot(perf,col="blue",lwd=2)


#random forest
set.seed(3221)
train<-sample(1:nrow(Attr.df),nrow(Attr.df)*0.7)
test<-Attr.df[-train,]
fit.forest<-randomForest(Attrition~.,data=Attr.df[train,],importance=TRUE)
fit.forest
rfpreds<-predict(fit.forest,test,type='class')
rfprobs <-predict(fit.forest,test,type='prob')
table(rfpreds,test$Attrition)
#roc
rf.pred <- prediction(rfprobs[,2],cl_label) 
perf <- performance(rf.pred,"tpr","fpr")
plot(perf,col="orange",lwd=2)
importance(fit.forest)
varImpPlot(fit.forest)


library(e1071)
set.seed(3221)
train<-sample(1:nrow(Attr.df),nrow(Attr.df)*0.7)
test<-Attr.df[-train,]
svm<-svm(Attrition~.,data=Attr.df[train,],probability = T,decision.values=T)
summary(svm)
svm.pred <-predict(svm,na.omit(test),probability = F,decision.values=F)
table(svm.pred,test$Attrition)
#predict.svm
svm.probs <-predict(svm,na.omit(test),probability = T,decision.values=F)
attr(svm.probs, "probabilities")[,]
#roc
svm.preds <- prediction(attr(svm.probs, "probabilities")[,2],cl_label) 
perf <- performance(svm.preds,"tpr","fpr")
plot(perf,col="turquoise",lwd=2)
legend("right",c("Logistics","LDA","KNN","Decision Tree","Random Forest","SVM","GBM_Upper"),
       col=c("violet","chocolate","green","blue","orange"," turquoise","antiquewhite4"),
       cex=0.8,bty='n',lty=1,lwd=2)

library(rminer)
model = fit(Attrition~.,data=Attr.df[train,],model = "svm")
VariableImportance = Importance(model,data=Attr.df[train,],method = "sensv")

L = list(runs = 1,sen = t(VariableImportance$imp),sresponses = VariableImportance$sresponses)
mgraph(L,graph = "IMP",leg = names(Attr.df[train,]),col = "gray",Grid = 10)



