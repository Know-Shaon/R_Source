#Distribution of random Variables 

#Create a sample of 50 random numbers from a normal distribution.
y <- rnorm(50)  #Takes random numbers from a normaldistribution where mean=0 SD-1
D = density(y)
plot(D)   #Observe the distribution as nearly normal 

#we can mention mean and SD explicitly as below a
y <- rnorm(50,mean=0,sd=3)
D = density(y)
plot(D)

#Create a normal distribution with your own range of values 
plot(dnorm(seq(-25,25,.2),mean=0,sd=10))
qnorm(.65,mean=0,sd=1)  # z-score at a specific Percentile 
pnorm(3)  # Percentile at a specific z-score

# Create a sequence of numbers between -10 and 10 incrementing by 0.1.
x <- seq(-10, 10, by = .1)
# Choose the mean as 2.5 and standard deviation as 0.5.
y <- dnorm(x, mean = 2.5, sd = 0.5)
plot(x,y)

# Create a sequence of numbers between -10 and 10 incrementing by 0.2.
x <- seq(-10,10,by = .2)
# Choose the mean as 2.5 and standard deviation as 2.
y <- pnorm(x, mean = 0, sd = 2)
plot(x,y)

#Create a sequence of probability values incrementing by 0.02.
temp<-y   # from the above block
# Choose the mean as 2 and standard deviation as 3.
y <- qnorm(temp, mean = 0, sd = 2)  #Returns quantile or the Z-Score
# Plot the graph.
plot(x,y)






# In t/student distribution, depending on the 
# degree of freedom we need to travel more distance (around the mean)
# than in normal distrubution to cover x% of the observations 
#----------------------------
#Student distribution 
qt(.95,df=50)  # z-score at a specific Percentile 
pt(2,df=30)  # Percentile at a specific z-score


#How Normal and Student distribution are different 
#---------------------------
pnorm(2, lower.tail = FALSE)
pt(2, df = 50, lower.tail = FALSE)

pnorm(2)
pt(2,df =5)





#---------------------
#Confidence interval 
#----------------------
#The sd of the 'average week-end sales distribution' is 65 and the mean is 130,considering 45 sample used for the experiment.
#With 95 confidence interval , what is the minimum and maximum sales values on nay day.

ME=t-score*SE   # t-score for 95% confidence interval is 1.654

qt(.95,df=45)  # find the t-score for 95% confidance interwal . It is 1.679427

ME=1.679427*(65/sqrt(45))
=1.679427*(65/6.708204)
=1.679427*9.689628
ME=16.27302
confidance interval =(mean-me, mean+me)
=(130-16.27302,130+16.27302)





#----------------------
#siple Hypothesis test 
#----------------------
#It was observe that, the sales of a product was sold 150 units on average after recruting a SE.
#The sd of the distribution is 65 and the mean is 130, considering 45 sample used for the experiment.
#Perform a hypothesis test in order to find out the significance of the sales ans Reward the executive.
SE=65/(sqrt(45))
t=(150-130)/SE
t
pt(t, df = 45, lower.tail = FALSE)    






#------------------
#POwer calculation 
#------------------
#Continuation to above problem.
#The sales manager anyway anticipated an average sales growth of 140 units , 
#what is the power of test in order to perform the hypothesis test to evaluate the performance of SE.
#Current mean 130
CM=130
#Antisipated 140
AM=140
#Shift required 
SR=(AM-CM)
SE=(65/(sqrt(45)))
Z=SR/SE
#Percentile at SR
pt(Z,df=45)
#Test requires 0.8462138% power

# Perform the test with the above power
SE=sqrt((65/(sqrt(45)))^2+(65/(sqrt(45)))^2)
t=(150-140)/SE
t
pt(t, df = 45, lower.tail = FALSE)    







#------------------------
#Binomial distribution     
y = rbinom(5000,10,.5) #Take 10 coins and give 5000 trials 
D = density(y)
plot(D)  
plot(dbinom(1:100,100,.5))  # Density from 1 success to 100 successes 

#Gender discrimination test with binomial distribution 
y = pbinom(1,48,.5)
y
#----Binomial distribution
# Create a sample of 50 numbers which are incremented by 1.
x <- seq(0,50,by = 1)
# Create the binomial distribution.
y <- dbinom(x,50,0.5)
# Plot the graph for this sample.
plot(x,y)
# Probability of getting 26 or less heads from a 51 tosses of a coin.
x <- pbinom(26,51,0.5)
print(x)
# How many heads will have a probability of 0.25 will come out when a coin is tossed 51 times.
x <- qbinom(0.2,51,1/2)
print(x)
# [1] 22  -->With the probability .2 ,the number of success at least we get by tossing 51 coins.





#-----Chi-Square Test(Relation between tow categorical variables)
#Chi-Square test is a statistical method to determine if two categorical
#variables have a significant correlation between them

# Load the library.
library("MASS")
# Create a data frame from the main data set.
car.data <- data.frame(Cars93$AirBags, Cars93$Type)
# Create a table with the needed variables.
car.data = table(Cars93$AirBags, Cars93$Type)
print(car.data)
# Perform the Chi-Square test.
print(chisq.test(car.data))







#------Analysis of covariance (categorical variable VS continues.Analysis of Covariance also called as ANCOVA.)
#Model with interaction between categorical variable and predictor variable

# mtcars- Data set described in the below link 
#http://www.sthda.com/english/wiki/r-built-in-data-sets#mtcars-motor-trend-car-road-tests
input <- mtcars
# Create the regression model.
result <- aov(mpg~am*hp,data = input)
print(summary(result))

#Model without interaction between categorical variable
# Get the dataset.
input <- mtcars
# Create the regression model.
result <- aov(mpg~hp+am,data = input)
print(summary(result))

#Comparing Two Models
# Get the dataset.
input <- mtcars

# Create the regression models.
result1 <- aov(mpg~hp*am,data = input)
result2 <- aov(mpg~hp+am,data = input)
# Compare the two models.
print(anova(result1,result2))
library(ggplot2)
input$am=as.factor(input$am)
ggplot(input,aes(x=hp,fill=am))+geom_density()
ggplot(input,aes(x=mpg,fill=am))+geom_density()


#Experiment for ANOVA 
x=rnorm(100,mean=15,sd=10)
dist1=data.frame(val=x,dist=1)


x1=rnorm(100,mean=5,sd=10)
dist2=data.frame(val=x1,dist=2)


my_data=rbind(dist1,dist2)
my_data$V2=as.factor(my_data$dist)
ggplot(my_data,aes(x=val,fill=V2))+geom_density()

result <- aov(val~V2,data = my_data)
summary(result)





#Simulation inference on small sample proportion
-----------------------------
##Paul the Octopus predicted 8 World Cup games, and predicted them all
##correctly. Does this provide convincing evidence that Paul actually has psychic
##powers, i.e. that he does better than just randomly guessing?

# H0: p = 0.5
# HA: p > 0.5
# 1. independence:
#   2. sample size / skew:
#   we can assume that his guesses are independent
# n = 8 x 0.5 = 4 ?? not met
# p = 1 distribution of sample proportions cannot be
# assumed to be nearly normal
source("http://bit.ly/dasi_inference")
paul = factor(c(rep("yes", 8), rep("no", 0)), levels = c("yes","no"))
inference(paul, est = "proportion", type = "ht", method = "simulation",success = "yes", null = 0.5, alternative = "greater")

