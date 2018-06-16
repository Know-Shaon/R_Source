#----------------------------------------------------
#Sigmoid function 

sigmoid = function(x) {
  return(1/(1+exp(-x)))
}

x=seq(-6,6,0.01)
plot(x, sigmoid(x), col='blue')

#----------------------------------------------------
#Source
#https://rpubs.com/aelhabr/logistic-regression-tutorial

install.packages("ISLR")
install.packages("tibble")
install.packages("pROC")
library("ISLR")
library("tibble")
library(gridExtra)
library(dplyr)
library(tidyr)
str(Default)
summary(Default)
#--------------------------------------------------------------------------
#Data Exploration --- Summary statistics and visualization 
#--------------------------------------------------------------------------
library(ggplot2)
p1=ggplot(Default,aes(x=balance,fill=default))+geom_density()

p2=ggplot(Default,aes(x=student,fill=default))+geom_bar()

p3=ggplot(Default,aes(x=income,fill=default))+geom_density()

grid.arrange(p1,p2,p3,ncol=2)

p1=ggplot(Default %>% filter(default=='No'),aes(x=income))+geom_density()
p2=ggplot(Default %>% filter(default=='Yes'),aes(x=income))+geom_density()
grid.arrange(p1,p2,ncol=1)


Default%>% filter(default=='Yes') %>% summarise(mean(balance),variance=var(balance),sd=sd(balance))
Default%>% filter(default=='No') %>% summarise(mean(balance),variance=var(balance),sd=sd(balance))

Default%>% filter(default=='Yes') %>% summarise(mean(income),variance=var(income),sd=sd(income))
Default%>% filter(default=='No') %>% summarise(mean(income),variance=var(income),sd=sd(income))
#--------------------------------------------------------------------------




#--------------------------------------------------------------------------
#Significance test
#--------------------------------------------------------------------------
#cor(Default$income,Default$balance)
test=aov(income ~ default,data=Default)
summary(test)

test=aov(balance ~ default,data=Default)
summary(test)

library("MASS")
print(chisq.test(table(Default$student,Default$default)))
#--------------------------------------------------------------------------





#--------------------------------------------------------------------------
#Sampling 
#--------------------------------------------------------------------------
# Split into train/test splits first.
set.seed(42)
default_idx <- sample(nrow(Default), 2*ceiling(nrow(Default) / 3))
default_trn <-  Default[default_idx, ]
default_tst <- Default[-default_idx, ]

#--------------------------------------------------------------------------
#Build model 
#--------------------------------------------------------------------------
model_glm <- glm(default ~ balance+student, data = default_trn, family = "binomial")
summary(model_glm)


#Prediction 
trn_pred <- ifelse(predict(model_glm, type = "response") > 0.5, "Yes", "No")
head(trn_pred)


# Making predictions on the train set.
trn_prob <- predict(model_glm, newdata = default_trn, type = "response")
trn_tab <- table(predicted = trn_pred, actual = default_trn$default)
trn_tab


# Making predictions on the test set.
tst_pred <- ifelse(predict(model_glm, newdata = default_tst, type = "response") > 0.5, "Yes", "No")
tst_tab <- table(predicted = tst_pred, actual = default_tst$default)
tst_tab


# Build a function for Error 
calc_class_err <- function(actual, predicted) {
  mean(actual != predicted)
}


calc_class_err(actual = default_trn$default, predicted = trn_pred)



# Test error rate should be close to train error rate if model is fitted properly.
calc_class_err(actual = default_tst$default, predicted = tst_pred)







#--------------------------------------------------------------------------
# Model evaluation more deeper 
library("caret")
confusionMatrix(trn_tab, positive = "Yes")



library("pROC")
test_prob <- predict(model_glm, newdata = default_tst, type = "response")
test_roc <- roc(default_tst$default ~ test_prob, plot = TRUE, print.auc = TRUE)


install.packages("InformationValue")
library(InformationValue)
#ks_plot(y_act, y_pred)
#ks_plot(default_tst$default,test_prob)
ks_stat(default_trn$default,trn_prob, returnKSTable = T)
ks_plot(default_trn$default,trn_prob)

# Follow the below link to understad this chart
#https://www.machinelearningplus.com/machine-learning/evaluation-metrics-classification-models-r/




library(ggplot2)
slope <- coef(model_glm)[2]/(-coef(model_glm)[3])
intercept <- coef(model_glm)[1]/(-coef(model_glm)[3]) 
ggplot(Default,aes(x=balance,y=student,size=default,color=default))+geom_jitter()+geom_abline(intercept=intercept,slope=slope)
  






#--------------------------------------------------------------------------
# Want to understand how the decision boundary is made ?
# Reference Is below
#https://stats.stackexchange.com/questions/6206/how-to-plot-decision-boundary-in-r-for-logistic-regression-model
#--------------------------------------------------------------------------

x1 <- rnorm(20, 1, 2)
x2 <- rnorm(20)

y <- sign(-1 - 2 * x1 + 4 * x2 )

y[ y == -1] <- 0

df <- cbind.data.frame( y, x1, x2)

mdl <- glm( y ~ . , data = df , family=binomial)

slope <- coef(mdl)[2]/(-coef(mdl)[3])
intercept <- coef(mdl)[1]/(-coef(mdl)[3]) 

library(lattice)
xyplot( x2 ~ x1 , data = df, groups = y,
        panel=function(...){
          panel.xyplot(...)
          panel.abline(intercept , slope)
          panel.grid(...)
        })
