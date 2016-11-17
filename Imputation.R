#List of R Packages for imputation
#1.MICE
#2.Amelia
#3.missForest
#4.Hmisc
#5.mi

#missForest
install.packages("missForest")
library(missForest)
install.packages("mice")
##-------------------------------------------------------------##
#install package and load library
install.packages("Hmisc")
library(Hmisc)
#load data
data("iris")
#seed missing values ( 10% )
iris.mis <- prodNA(iris, noNA = 0.1)
summary(iris.mis)

# impute with mean value
iris.mis$imputed_age <- with(iris.mis, impute(Sepal.Length, mean))

# impute with random value
iris.mis$imputed_age2 <- with(iris.mis, impute(Sepal.Length, 'random'))

#similarly you can use min, max, median to impute missing value

#using argImpute
impute_arg <- aregImpute(~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width +
                             Species, data = iris.mis, n.impute = 5)
#argImpute() automatically identifies the variable type and treats them accordingly.
impute_arg

#check imputed variable Sepal.Length
impute_arg$imputed$Sepal.Length
Final<- merge(iris,impute_arg$imputed$Species,by=0)





#S T E P --1
# prepare a data set to impute
#seed missing values ( 10% )
iris.mis <- prodNA(iris, noNA = 0.1)
summary(iris.mis)


#S T E P --2
#------------------------------------------------------------------------
#adding dummy variables to iris.mis data
#------------------------------------------------------------------------
Species_d=model.matrix(~ Species - 1, data=iris.mis)
temp=merge(iris.mis,species_dummy,by= 0,all='TRUE')
cols=c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Speciessetosa"    
       ,"Speciesversicolor","Speciesvirginica" )
iris.mis1=temp[cols]

#S T E P --3
# IMPUTATION 
#------------------------------------------------
impute_arg <- aregImpute(~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width +
                           Speciessetosa + Speciesversicolor + Speciesvirginica , data = iris.mis1, n.impute = 5)

#S T E P --4
#below is the imputed_data
##impute_arg$imputed$Sepal.Length
##impute_arg$imputed$Sepal.Width
##impute_arg$imputed$Petal.Width
##impute_arg$imputed$Species
test=impute(iris.mis1,1,impute_arg$imputed$Sepal.Length,Catogorical='False')
test=impute(test,2,impute_arg$imputed$Sepal.Width,Catogorical='False')
test=impute(test,3,impute_arg$imputed$Petal.Length,Catogorical='False')
test=impute(test,4,impute_arg$imputed$Petal.Width,Catogorical='False')
test=impute(test,5,impute_arg$imputed$Speciessetosa,Catogorical='True')
test=impute(test,6,impute_arg$imputed$Speciesversicolor,Catogorical='True')
test=impute(test,7,impute_arg$imputed$Speciesvirginica,Catogorical='True')





#-------------------------------------
# Imputation with mice package
#-------------------------------------
#https://www.r-bloggers.com/imputing-missing-data-with-r-mice-package/
#-------------------------------------
library(missForest)
library(Hmisc)
#load data
data("iris")
# prepare a data set to impute
#seed missing values ( 10% )
iris.mis <- prodNA(iris, noNA = 0.1)
summary(iris.mis)

#subsetting complete and !complete data
iris.mis[complete.cases(iris.mis),]
iris.mis[!complete.cases(iris.mis),]

library(mice)
md.pattern(iris.mis)

tempData <- mice(iris.mis,m=5,maxit=50,meth='pmm',seed=500) #meth='pmm'
summary(tempData)

#Features imputes are below. sice m=5 , there are five imputed datasets
tempData$imp$Sepal.Width
tempData$imp$Sepal.Length
tempData$imp$Petal.Length
tempData$imp$Petal.Width

#to see the method used
tempData$meth

completedData <- complete(tempData,1)



#----------------------------------------------------------------------------------------
#some useful functions
#----------------------------------------------------------------------------------------
#df1,--- Base Data frame 
#df2,--- Data frame to look
#what_tolook-- What to look for a match
#what_toget----What to get finally (suffix with .x or .y depending on the columns to get)
#Lookup function
lookup_table <- function(df1,df2,what_tolook,what_toget){
  temp_df = merge(df1,df2,by=what_tolook)
  ret_df  = temp_df[what_toget]
  return(ret_df)
}



#--------------------------------------------
#Function to replace the Na cell of data frame with multiple imputed datasets
#The function finds mean or mode of multiple imputed data sets, and replaces NAs
#--------------------------------------------
#df1--Data frame
#col--Col number
#imputed_data---impued data contains single column from multiple datasets(imputed data sets)
#--------------------------------------------
impute <-  function(df1,col,imputed_data,Catogorical='True') {
  for (i in 1:nrow(df1)) {
    if (is.na(df1[i,col])){  
      r_name <- i
      for(j in 1:nrow(imputed_data)){
        if (row.names(imputed_data)[j] == r_name){
          if (Catogorical=='True'){
            rep_val<-getmode(imputed_data[j,])
            df1[i,col]<-rep_val
            print(rep_val)
            break
          }else{rep_val<-mean(imputed_data[j,]) 
          print('test')
          df1[i,col]<-rep_val
          print(i)
          print(rep_val)  
          break
          }     
        }
      }
    }  
  }
  return(df1)
}


#------------------------------------------------------------
#function to get the mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#-----------------------------------------------------------
#To delete a column
df[[2]]<- NULL   # will delete the secod column


