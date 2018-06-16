#Reference
# https://rpubs.com/emzak208/281776
# https://www.kdnuggets.com/2016/04/association-rules-apriori-algorithm-tutorial.html

#install.packages(arules)
library(arules)
library(arulesViz)


#Read Transactions
groceries <- read.transactions("http://www.sci.csueastbay.edu/~esuess/classes/Statistics_6620/Presentations/ml13/groceries.csv", sep = ",")
summary(groceries)


#look at the first five transactions
inspect(groceries[1:5])

#examine the frequency of items
itemFrequency(groceries[, 1:3])

# Top 20 frequencies
itemFrequencyPlot(groceries, topN = 20)

#plot the frequency of items
itemFrequencyPlot(groceries, support = 0.1)


#a visualization of the sparse matrix for the first five transactions
image(groceries[1:5])


#visualization of a random sample of 100 transactions
image(sample(groceries, 100))


#-------------------------------------------------------------------------
#training a model on the data
#-------------------------------------------------------------------------
#  default settings result in zero rules learned
# apriori(groceries)

#set better support and confidence levels to learn more rules
groceryrules <- apriori(groceries, parameter = list(support =
                                                      0.006, confidence = 0.25, minlen = 2))
groceryrules

# summary of grocery association rules
summary(groceryrules)

#look at the first three rules
inspect(groceryrules[1:3])





#--------------------------------------------------------------------
# Model Performance and sorting rules by list 
#--------------------------------------------------------------------
inspect(sort(groceryrules, by = "lift")[1:5])

#subsetting
berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)

#Writing the rules to a CSV file
write(groceryrules, file = "groceryrules.csv",sep = ",", quote = TRUE, row.names = FALSE)




#--------------------------------------------------------------------
#Visualization 
#--------------------------------------------------------------------

library(arules)
plot(groceryrules,method="graph",interactive=FALSE,shading=NA)
