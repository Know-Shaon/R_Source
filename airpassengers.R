
#Reference
#https://rpubs.com/emb90/137525

library(readr)
AirPassengers <- read_csv("C:/Users/v m kishore/OneDrive/Data sets/air-passengers/AirPassengers.csv", 
                          col_types = cols(Month = col_character()))
#View(AirPassengers)
#Creating a time series object
#ts(vector, start=, end=, frequency=) 
AirPassengers<- ts(AirPassengers$`#Passengers`,start=c(1949,1),frequency=12)


# Rinbuild dataset
#data("AirPassengers")

AP <- AirPassengers
plot(AP, ylab="Passengers (1000s)", type="o", pch =20)


AP.decompM <- decompose(AP, type = "multiplicative")
plot(AP.decompM)



t <- seq(1, 144, 1)
modelTrend <- lm(formula = AP.decompM$trend ~ t)
predT <- predict.lm(modelTrend, newdata = data.frame(t))
plot(AP.decompM$trend[7:138] ~ t[7:138], ylab="T(t)", xlab="t",
     type="p", pch=20, main = "Trend Component: Modelled vs Observed")
lines(predT, col="red")



#Is it a best fit?
plot(modelTrend)


summary(modelTrend)


#Create trend cpmponent for the next one year
Data1961 <- data.frame("T" = 2.667*seq(145, 156, 1) + 84.648, S=rep(0,12), e=rep(0,12),
                       row.names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
Data1961  #This is a new data set created with predicted components 


#Sesonal component 
AP.decompM$seasonal



Data1961$S <- unique(AP.decompM$seasonal)
Data1961


#Random Error
plot(density(AP.decompM$random[7:138]),
     main="Random Error")

Data1961$e <- 1
Data1961

sd_error <- sd(AP.decompM$random[7:138])
#sd_error <- sd_error/(sqrt(131))
sd_error

Data1961$R <- Data1961$T * Data1961$S * Data1961$e                  #Realistic Estimation
Data1961$O <- Data1961$T * Data1961$S * (Data1961$e+1.95*sd_error)  #Optimistic Estimation
Data1961$P <- Data1961$T * Data1961$S * (Data1961$e-1.95*sd_error)  #Pessimistic Estimation
Data1961



xr = c(1,156)
plot(AP.decompM$x, xlim=xr, ylab = "Passengers (100s)", xlab = "Month")
lines(data.frame(AP.decompM$x))

lines(Data1961$R, x=seq(145,156,1), col="black")
lines(Data1961$O, x=seq(145,156,1), col="green")
lines(Data1961$P, x=seq(145,156,1), col="red")


