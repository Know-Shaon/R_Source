#Reference
#https://rstudio-pubs-static.s3.amazonaws.com/40602_03881f4466844aed83feb45f48444906.html


require(ggplot2)
require(knitr)
set.seed(7759)

#-----------------------------------------------
# Create a non-uniform population of 100,000 numbers between 1 and 100
pop1 <- rnorm(20000, mean = 10, sd = 3)
pop2 <- rnorm(80000, mean = 70, sd = 10)
pop <- c(pop1, pop2)

mu <- mean(pop) #calculate the population mean
sigma <- sd(pop) #calculate the population standard deviation
rm(pop1, pop2) #clean up


#-----------------------------------------------
popdf <- as.data.frame(pop)
hg <- ggplot(popdf, aes(x = pop)) + geom_histogram(colour = "black", fill = "steelblue") + 
  ggtitle("Histogram of Population") + xlab("value")
hg

#-----------------------------------------------

n <- c(1, 5, 10, 30, 50, 100) #set up number of samples
t <- c(10, 100, 1000, 10000) #set up number of trials in simulation

df <- data.frame() #initialize our empty data frame

# Run the simulation
for(i in n) { #for each value of n...
  col <- c()
  for(j in t) { #we loop through each value of t...
    trial <- 1:j
    counter <- j #set up an egg timer based on whichever t value we're on
    value <- c()
    while(counter > 0) {    # and extract n samples from the population...
      bucket <- sample(pop, i, replace = TRUE)
      xbar <- mean(bucket) #calculate the mean...
      value <- c(value, xbar) # and add it to a vector
      counter <- counter - 1 #egg timer counts down and loops back until it hits 0
    }
    sbar <- sd(value) #calculate the standard deviation of our sample
    col <- cbind(trial, value, sbar, i, j) #stick all the info together...
    df <- rbind(df, col) #and attach it to our master data frame
  } #and we do it again for the next set of values until we're done!
  
}

rm(col, bucket, value, counter, i, j, n, sbar, t, xbar, trial) #clean up

# Let's take a look!
str(df)


#--------------------------------------------------------------------------------------

head(df, n = 25) #the full table is too big to look at but we can take a peek at the first few rows.


#--------------------------------------------------------------------------------------
#We tidy up our data frame to get it ready for graphing. Note that we built it in "tall"
# form so it's already structured for ggplot

names(df) <- c("trial#", "value", "sdev", "samples", "trials")

# Creating the plot
g <- ggplot(df, aes(x = value)) + geom_density(fill = "steelblue") + 
  facet_grid(samples ~ trials, labeller = label_both) + 
  ggtitle("Demonstrating The Central Limit Theorem With Simulation") + 
  geom_vline(xintercept = mu, linetype = "dashed")
g
