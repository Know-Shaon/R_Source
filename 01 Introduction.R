#######E X C E R S I Z E -1##################*
#####################################*

#R functions
dataframe_name=data.frame(array1,array2,array3)
seq(1,10)     # displays the value in sequence 
rbind(<dataframe1,dataframe2)
array=c("a","b")

#Loading data
getwd()     #get the current working directory
WHO=read.csv("WHO.csv")
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
tapply(WHO$Over60, WHO$Region, mean)  #displays mean order by region


#Plot data
plot(WHO$GNI,WHO$Fertilityrate)  # this will display a dot plot
outliers=subset(WHO, GNI>10000 & FertilityRate>2.5)   # Sub setting the outliers
nrow(outliers)   #displays rows
outliers[c('Country','GNI','FertilityRate')]   # create a vector and display data.
hist(WHO$CellularSubscribers)   # to plot a histogram
boxplot(WHO$LifeExpectancy ~ WHO$Country, xlab=' ',ylab='Life Expectancy',Main='LifeExpectancy by Country')   # to view the statistical range of the data.


#Handling missing data
tapply(WHO$LiteracyRate, WHO$Region, min, na.rm='True')    # this is the way we can exclude missing data from datset.

#######E X C E R S I Z E -2##################*
#####################################*


#exploring data
getwd() 	#to see the working directory 
USDA = read.csv('USDA.csv')   #load data
summary(USDA)   # to show the summary of the numerical variables 
names(USDA)     # to retrieve the column names of the data frame
match("CAVIAR", USDA$Description)    # to find the matching description and returns the Index   
USDA$Sodium[4154]   # to find the level of sodium in Caviar
USDA$Sodium[match("CAVIAR", USDA$Description)]   # this also works as above 

# Working with plots 
plot(USDA$Protein, USDA$TotalFat)   #This will display a dot plot
#the plot shows a triangular shape which means the food that has protein doesn't have fat and vice versa.
hist(USDA$VitaminC, xlab='Vitamin C (mg)', main='Histogramof Vitamin C Levels') # show the frequency of VitaminC

#Xlim will specify the X-axis area where the histogram zoom into. Breaks will give the cell breakupsfor original interval   
hist(USDA$VitaminC, xlab='Vitamin C (mg)', main='Histogramof Vitamin C Levels',xlim=c(0,100),breaks=100)
# BOX Plots
boxplot(USDA$Sugar, main ='heading', ylab='sugur (g)')

#Adding variables to dataset 
#the Records in the data frame, that have higher sodium value than mean.
HighSodium = USDA$Sodium > mean(USDA$sodium , na.rm=True)   # HighSodium is a vector of Boolean 
HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium , na.rm='true'))  # this will be a numeric 1=true and 0=false.
str(USDA$HighSodium) # to display the values of vector.
USDA$HighFat= as.numeric(USDA$TotalFat > mean(USDA$TotalFat , na.rm='true'))  # adding column to data frame.
USDA$HighSodium
#similarly add the below vectors as well.
USDA$HighFat
USDA$HighCarbohidrate
USDA$HighProtein
USDA$HighSoudium
#how many foods have higher than average sodium value and lower than average sodium values.
table(USDA$HighSodium)
0	 	1
4884	2090
#show the number with high sodium and low fat and vice versa 
table(USDA$HighSodium, USDA$HighFat)
0		1
0	3529		1355
1	1378		712

# tapply function has 3 arguments, tapply(argument1, argument2, argument3)
# Group argumen1 by argument2 and apply argument 3
#find the mean iron values for high and low protein food
tapply(USDA$Iron, USDA$highprotein, mean,na.rm=True)


#######E X C E R S I Z E -3##################*
#####################################*

#Changing the Data type

is.numeric(), is.character(), is.vector(), is.matrix(), is.data.frame()
as.numeric(), as.character(), as.vector(), as.matrix(), as.data.frame()

#Removing duplicates 
unique(x)

#Merging Data frames
# merge two data frames by ID
total <- merge(data frameA,data frameB,by="ID")
# merge two data frames by ID and Country
total <- merge(data frameA,data frameB,by=c("ID","Country"))
#Appending Data frames
total <- rbind(data frameA, data frameB)

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
library(reshape)
mdata <- melt(mydata, id=c("id","time"))

# cast the melted data
# cast(data, formula, function) 
subjmeans <- cast(mdata, id~variable, mean)
timemeans <- cast(mdata, time~variable, mean)


#####Subsetting Data############################################
#http://www.statmethods.net/management/subset.html

#Selecting (Keeping) Variables
# select variables v1, v2, v3
myvars <- c("v1", "v2", "v3")
newdata <-mydata[myvars]


#Excluding (DROPPING) Variables
# exclude variables v1, v2, v3
myvars <- names(mydata) %in% c("v1", "v2", "v3") 
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
[1] 2 1 3 3

#Converting a factor to numeric without losing information R (as.numeric() doesn't seem to work)
blah<-c("4","8","10","15")
blah
blah.new<-as.factor(blah)
blah.new
blah.new1<-as.numeric(blah.new)
blah.new1
blah.new1<-as.numeric(as.character(blah.new))	


############################################
#Set operations
#https://stat.ethz.ch/R-manual/R-devel/library/base/html/sets.html
#http://adv-r.had.co.nz/Subsetting.html
############################################


