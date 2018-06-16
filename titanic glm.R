#install.packages("Amelia")
library(Amelia)
#install.packages('ROCR')
library(ROCR)
library(dplyr)
library(tidyr)
library(caret)
library(readr)
library(caTools)
titanic<- read_csv("C:/Users/v m kishore/OneDrive/Data sets/titanic/train.csv")
View(train)

summary(titanic)
missmap(titanic,main="missing values VS observed")
total_data<-select(titanic,-c(Cabin,Age,Name,Ticket))
summary(total_data)
str(total_data)

#Cleaning data
table(is.na(total_data))
total_data_clean<-total_data[complete.cases(total_data),]


#sampling data
total_data_clean=transform(total_data_clean,spl=sample.split(1:dim(total_data_clean)[1],SplitRatio=0.7))
train=subset(total_data_clean[,1:5], total_data_clean$spl==TRUE)   #the subset of iris dataset for which spl==TRUE
test=subset(total_data_clean[,1:5], total_data_clean$spl==FALSE)




#Building Model
names(train)
model<-glm(Survived ~ .,family=binomial(link='logit'),data=train)
model<-glm(Survived ~ .,data=train)
summary(model)
anova(model,test="Chisq")

#-------------------------
#Cross Validation or Model evaluation
summary(test)
summary(test)
str(test)

p<-predict(model,newdata = test)
p <- ifelse(p > 0.5,1,0)

## Confusion matrix and statistics
#library(caret)
#confusionMatrix(data=result, reference=test$Survived)
confusionMatrix<-table(p,test$Survived)
print(confusionMatrix)



## ROC Curve and calculating the area under the curve(AUC)
library(ROCR)
predictions <- predict(model, newdata=test, type="response")
ROCRpred <- prediction(predictions, test$Survived)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1))

auc <- performance(ROCRpred, measure = "auc")
auc <- auc@y.values[[1]]
auc



# End---------
rm(list=ls())
