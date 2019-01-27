#Load WHO Data
#Load USDA data

#Exercise on WHO Data  ------1
#R functions
#Loading data
#Subsetting the data
#Data analysis using R
#Plot data (DOT, BOX, HIST)
#Handling missing data



#Exercise on USDA Data  ------2
#Exploring data
#Working with plots (DOT HIST BOX)
#Adding variables to dataset 

#load packages
#install.packages("ggplot2")
library(ggplot2)

#Loading data
library(readr)
WHO <- read_csv("WHO.csv", 
                col_types = cols(FertilityRate = col_number(), 
                                 GNI = col_number(), LiteracyRate = col_number(), 
                                 PrimarySchoolEnrollmentFemale = col_number(), 
                                 PrimarySchoolEnrollmentMale = col_number()))


#Loading data
getwd()     #get the current working directory
#WHO=read.csv("WHO.csv")
str(WHO)    # to display the data from R
#factor is a categorical data that repeats 
#numerical summary 
summary(WHO)

#Subsetting the data
WHO_Eup=subset(WHO, Region=="Europe")
str(WHO_Eup)
write.csv(WHO_Eup, "WHO_eup.csv")

ls()   # to see what variables you have in R
rm(WHO_Eup)   #WHO_Eup has gone!
ls()   # now you should not see the WHO_Eup data set

#Data analysis using R
WHO$Under15 # this is how R recognizes a column of a data frame
mean(WHO$Under15)  # will give the mean, you can also use sd(),summart() functions on a column
which.min(WHO$Under15)   # returns the index of the row that has minimum value.
WHO$Country[86]    # access the row with the column
table(WHO$Region)  #Group and sum the column  
tapply(WHO$Over60, WHO$Region, mean)  #displays mean prder by region


#Plot data
plot(WHO$GNI,WHO$FertilityRate)  # this will display a dot plot
outliers=subset(WHO, GNI>60000, FertilityRate>2.5)   # Sub setting the outliers
nrow(outliers)   #displays rows

outliers[c('Country','GNI','FertilityRate')]   # create a vector and display data.
hist(WHO$CellularSubscribers)   # to plot a histogram
boxplot(WHO$LifeExpectancy ~ WHO$Country, xlab=' ',ylab='Life Expectancy',Main='LifeExpectancy by Country')   # to view the statistical range of the data.
ggplot(aes(y=LifeExpectancy,x=Region),data=WHO)+geom_boxplot()

#Handling missing data
tapply(WHO$LiteracyRate, WHO$Region, min, na.rm='True')    # this is the way we can exclude missing data from datset.

#######E X C E R S I Z E -2##################*
#####################################*


#exploring data
#getwd() 	#to see the working directory 
#USDA = read.csv('USDA.csv')   #load data

#loading Data into R
#library(readr)
USDA <- read_csv("USDA.csv")
View(USDA)

summary(USDA)   # to show the summary of the numerical variables 
names(USDA)     # to retrieve the column names of the data frame
match("CAVIAR", USDA$Description)    # to find the matching description and returns the Index
USDA$Sodium[4154]   # to find the level of sodium in Caviar
USDA$Sodium[match("CAVIAR", USDA$Description)]   # this also works as above 

# Working with plots 
plot(USDA$Protein, USDA$TotalFat)   #This will display a dot plot
#the plot shows a triangular shape which means the food that has protein doesn't have fat and vice versa.
hist(USDA$VitaminC,xlab='Vitamin C (mg)', main='Histogramof Vitamin C Levels') # show the frequency of VitaminC

#Xlim will specify the X-axis area where the histogram zoom into. Breaks will give the cell breakupsfor original interval   
hist(USDA$VitaminC, xlab='Vitamin C (mg)', main='Histogramof Vitamin C Levels',xlim=c(0,100),breaks=1000)
# BOX Plots
boxplot(USDA$Sugar, main ='heading', ylab='sugur (g)')

#Adding variables to dataset 
#the Records in the data frame, that have higher sodium value than mean.
HighSodium = USDA$Sodium > mean(USDA$Sodium , na.rm='True')   # HighSodium is a vector of Boolean 
USDA$HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium , na.rm='true'))  # this will be a numeric 1=true and 0=false.
table(USDA$HighSodium) # to display the values of vector.
USDA$HighFat= as.numeric(USDA$TotalFat > mean(USDA$TotalFat , na.rm='true'))  # adding column to data frame.
table(USDA$HighFat)

#similarly add the below vectors as well.
USDA$HighFat
USDA$HighCarbohidrate
USDA$HighProtein
USDA$HighSoudium
#how many foods have higher than average sodium value and lower than average sodium values.
table(USDA$HighSodium)

#show the number with high sodium and low fat and vice versa 
table(USDA$HighSodium, USDA$HighFat)

# tapply function has 3 arguments, tapply(argument1, argument2, argument3)
# Group argumen1 by argument2 and apply argument 3
#find the mean iron values for high and low protein food
tapply(USDA$Iron, USDA$highprotein, mean,na.rm=True)

#######################################
# Excersize ---3
#######################################
# Find Mean.
result.mean <- mean(WHO$Population)
print(result.mean)

# Find Mean.
result.mean <-  mean(WHO$Population,trim = 0.3) # Vector will be sorted and 3 variable left and right will be trimmed.
print(result.mean)

# Find mean dropping NA values.
result.mean <-  mean(WHO$Population,na.rm = TRUE)  # Remove not available values
print(result.mean)


# Find the median.
median.result <- median(WHO$Population)
print(median.result)

# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}




#----Linear regression
x=USDA$Calories
y=USDA$Carbohydrate
relation <- lm(y~x)

# Give the chart file a name.
png(file = "linearregression.png")

# Plot the chart.
plot(y,x,col = "blue",main = "Calories & Carbohydrate Regression",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "Calories",ylab = "Carbohydrate")

# Save the file.
dev.off()
ggplot(aes(x=Calories,y=Carbohydrate),data=USDA)+geom_smooth()+geom_abline(slope=,06,intercept = 5.37)
ggplot(aes(x=Calories,y=Carbohydrate),data=USDA)+geom_smooth()+geom_point()

#-----Logistic regression
am.data = glm(formula = am ~ cyl + hp + wt, data = input, family = binomial)
print(summary(am.data))


#----RandomForest
install.packages("randomForest")
library(randomForest)
randomForest(formula, data)

#----Decision Trees
install.packages("party")
ctree(formula, data)

#----TimeSeries with R
# Get the data points in form of a R vector.
rainfall1 <- c(799,1174.8,865.1,1334.6,635.4,918.5,685.5,998.6,784.2,985,882.8,1071)
rainfall2 <-
  c(655,1306.9,1323.4,1172.2,562.2,824,822.4,1265.5,799.6,1105.6,1106.7,1337.8)
# Convert them to a matrix.
combined.rainfall <-  matrix(c(rainfall1,rainfall2),nrow = 12)
# Convert it to a time series object.
rainfall.timeseries <- ts(combined.rainfall,start = c(2012,1),frequency = 12)
# Print the timeseries data.
print(rainfall.timeseries)



#######E X C E R S I Z E -4##################*
#####################################*

#Changing the Data type

is.numeric(), is.character(), is.vector(), is.matrix(), is.data.frame()
as.numeric(), as.character(), as.vector(), as.matrix(), as.data.frame()

#Removing duplicates 
unique(x)

#Merging Data frames
# merge two data frames by ID
total <- merge(data_frameA,data_frameB,by="ID")
# merge two data frames by ID and Country
total <- merge(data_frameA,data_frameB,by=c("ID","Country"))
#Appending Data frames
total <- rbind(data_frameA, data_frameB)

#Sorting Data
# sorting examples using the mtcars dataset
attach(mtcars)
# sort by mpg
newdata <- mtcars[order(mpg),] 
# sort by mpg and cyl
newdata <- mtcars[order(mpg, cyl),]
#sort by mpg (ascending) and cyl (descending)
newdata <- mtcars[order(mpg, -cyl),] 
detach(mtcars)


# aggregate data frame mtcars by cyl and vs, returning means
# for numeric variables
attach(mtcars)
aggdata <-aggregate(mtcars, by=list(cyl,vs), FUN=mean, na.rm=TRUE)
print(aggdata)
detach(mtcars)


#The Reshape Package
# example using built-in dataset 
mtcars
t(mtcars)
# example of melt function 
install.packages("reshape")
library(reshape)
mydata <- melt(mtcars, id=c("gear","carb"))

# cast the melted data
# cast(data, formula, function) 
subjmeans <- cast(mydata,  formula =...~ variable, fun.aggregate=mean)
timemeans <- cast(mydata, time~variable, mean)


#####Subsetting Data############################################
#http://www.statmethods.net/management/subset.html
#Selecting (Keeping) Variables
# select variables v1, v2, v3
myvars <- c("v1", "v2", "v3")
newdata <-mydata[myvars]

#Excluding (DROPPING) Variables
#exclude variables v1, v2, v3
subset<- subjmeans[,names(subjmeans) %in% c("gear","carb","mpg")]
newdata <- mydata[!myvars]
# exclude 3rd and 5th variable 
newdata <- mydata[c(-3,-5)]

#Selecting Observations
# first 5 observations
newdata <- mydata[1:5,]
# based on variable values
newdata <- mydata[ which(mydata$gender=='F' 
                         & mydata$age > 65), ]

#Selection using the Subset Function
# using subset function 
newdata <- subset(mydata, age >= 20 | age < 10, 
                  select=c(ID, Weight))

#In the next example, we select all men over the age of 25 and we keep variables weight through income (weight, income and all columns between them).
# using subset function (part 2)
newdata <- subset(mydata, sex=="m" & age > 25,
                  select=weight:income)

#Random sample
# take a random sample of size 50 from a dataset mydata 
# sample without replacement
mysample <- mydata[sample(1:nrow(mydata), 50,	replace=FALSE),]



#####Working with Factors##########
#http://www.dummies.com/programming/r/how-to-convert-a-factor-in-r/
directions <- c("North", "East", "South", "South")
directions.factor <- factor(directions)
directions.factor
as.character(directions.factor)
#Use as.numeric() to convert a factor to a numeric vector
as.numeric(directions.factor)



############################################
#Set operations
#https://stat.ethz.ch/R-manual/R-devel/library/base/html/sets.html
#http://adv-r.had.co.nz/Subsetting.html
############################################
