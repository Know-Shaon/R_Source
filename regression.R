#Correlation Explained
#https://www.analyticsvidhya.com/blog/2017/06/a-comprehensive-guide-for-linear-ridge-and-lasso-regression/
  

#------------------------------------------------------------------
#Correlation between any random variable 
#------------------------------------------------------------------
#OpenIntro-page 339

x <- rnorm(50)
y <- rnorm(50)
cor(x,y)
scatter.smooth(x=x,y=y)

#Positive correlation 
x=rnorm(50)
y=2*x+3
cor(x,y)
scatter.smooth(x=x,y=y)
#add some noise 
y=y+rnorm(length(y),mean(y),sd=sd(y))
cor(x,y)
scatter.smooth(x=x,y=y)


#Negative correlation 
x=rnorm(50)
y=-2*x+3
cor(x,y)
scatter.smooth(x=x,y=y)


# Is correlation is sensitive to outliers ?
x=c(1,2,3,4,5,6,7)
#y=c(2,4,6,16,25,36)
y=c(2,4,6,8,10,12,14)  # squared values of x
cor(x,y)

# an an out lier 
x=c(1,2,3,4,5,6,7)
y=c(2,4,6,8,10,12,1000)  # squared values of x
cor(x,y)






#------------------------------------------------------------------
#http://r-statistics.co/Linear-Regression.html
#------------------------------------------------------------------

#install.packages("DAAG")
library(DAAG)
View(cars)
scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")  # scatterplot
cor(cars$speed,cars$dist)



par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out))  # box plot for 'speed'
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))  # box plot for 'distance'





#-----------------------------
#Building a linear model 
#-----------------------------
linearMod <- lm(dist ~ speed, data=cars)  # build linear regression model on full data
print(linearMod)

#-----------------------------
#To diagnose the model 
#-----------------------------
summary(linearMod)  # model summary
par(mfrow=c(2, 2))
plot(linearMod)
#The below plots are used to evaluate the performance of regression 
#Reciduals VS Fitter
#Standardized Residuals VS Theoretical Quantiles
#sqrt(|Standardized Residuals|) VsFitted Values
#standardized residuals VS Leverage



#-----------------------------
#How to calculate the t Statistic and p-Values?
#-----------------------------
modelSummary <- summary(linearMod)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["speed", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["speed", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
p_value



#------------------------------------------------------------
#plolynomial Regression
#------------------------------------------------------------
#Sopurce 
#https://www.r-bloggers.com/fitting-polynomial-regression-in-r/
#linear relationship 
p <- 0.5
q <- seq(0,100,1)
y <- p*q
plot(q,y,type='l',col='red',main='Linear relationship')

#A non-linear relation ship 
rm(x,y)
x=seq(5,15,.1)
y=sin(x)
plot(x,y)
plot(x,y,type='l',col='navy',main='Nonlinear relationship',lwd=3)


#add noice to data
noise <- rnorm(length(y), mean=mean(y), sd=sd(y))
noisy.y <- y + noise
plot(x,noisy.y,col='navy',main='Nonlinear relationship',lwd=3)


#build a polynomial regression

library(ggplot2)
data=data.frame(x,y,noisy.y)
ggplot(aes(x=x,y=noisy.y),data=data)+geom_point()
ggplot(aes(x=x,y=noisy.y),data=data)+geom_point()+geom_smooth()
ggplot(aes(x=x,y=noisy.y),data=data)+geom_point()+geom_smooth(method=lm)


model <- lm(noisy.y ~ poly(x,5))
summary(model)
ggplot(aes(x=x,y=noisy.y),data=data)+geom_point()+geom_smooth(method = lm,formula =noisy.y ~ poly(x,15))



#What happens if we apply linear regression to a data , that has no-linear relationships
model <- lm(noisy.y ~ x)
summary(model)
plot(model)



#Model design for bios and variance problems.
rm(x,y)
x=seq(5,15,.1)
y=sin(x)
noise <- rnorm(length(y), mean=mean(y), sd=sd(y))
noisy.y <- y + noise
my_data=data.frame(noisy.y,x,x^2,x^3,x^4,x^5,x^6,x^7,x^8,x^9,x^10,x^11,x^12,x^13,x^14,x^15)

#Sampling take 60% as train data 40% as test data.
library(caret)
index <- createDataPartition(my_data$noisy.y, p=0.60, list=FALSE)
trainSet <- my_data[ index,]
testSet <- my_data[-index,]


#model under fitted 


#model over fitter 
#model <- lm(noisy.y ~ x^2+x^3+x^4+x^5+x^6+x^7+x^8+x^9+x^10+x^11+x^12+x^13+x^14+x^15)


#model rightly fitted 
model <- lm(trainSet$noisy.y ~ trainSet$x.2+trainSet$x.3+trainSet$x.4+trainSet$x.5)
names=c('x.2','x.3','x.4','x.5')
summary(model)


#validation 
y.predict=predict(model,testSet[2:5,])
