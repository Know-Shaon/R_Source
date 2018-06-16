#install.packages("e1071")
#install.packages("caTools")
#https://www.analyticsvidhya.com/blog/2017/09/naive-bayes-explained/
library(e1071)
library(caTools)

#load Dataset
library(readr)
iris_csv <- read_csv("C:/Users/v m kishore/OneDrive/Data sets/iris.csv.txt")
#View(iris_csv)
iris_csv$species=as.factor(iris_csv$species)

#Random data samples with proportion 
iris_csv$spl=sample.split(iris_csv,SplitRatio=0.7)

# By using the sample.split() we are creating a vector with values TRUE and FALSE and by setting
#the SplitRatio to 0.7, we are splitting the original Iris dataset of 150 rows to 70% training
#and 30% testing data. 
train=subset(iris_csv[,1:5], iris_csv$spl==TRUE)   #the subset of iris dataset for which spl==TRUE
test=subset(iris_csv[,1:5], iris_csv$spl==FALSE)

#bulding model & Evaluation .
model <- naiveBayes(species ~ ., data = train)
preds=predict(model, test[,-5])
conf_matrix <- table(preds,test$species)
conf_matrix

#actual classes
table(test$species)


