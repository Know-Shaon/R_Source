
HDFC Bank Limited


# https://www.investopedia.com/ask/answers/033015/how-does-stock-market-affect-gross-domestic-product-gdp.asp
# http://www.apeejay.edu/aitsm/journal/docs/issue-oct-2016/ajmst040104.pdf
# https://dbie.rbi.org.in/DBIE/dbie.rbi?site=home
# http://aircconline.com/ijdkp/V3N1/3113ijdkp06.pdf

library(readr)
cprice <- read_csv("C:/Users/v m kishore/OneDrive/Data sets/NSE/11-04-2016-TO-10-04-2018HDFCBANKALLN.csv", 
                                               col_types = cols_only(`Close Price` = col_guess()))



cprice<- ts(cprice$`Close Price`,start=c(2016,1),frequency = 324)
CP <- cprice
plot(CP, ylab="stock price (100s)", type="o", pch =10)



CP.decompM <- decompose(CP, type = "multiplicative")
plot(CP.decompM)



# Check for trend component
#-----------------------------
t <- seq(1, 321, 1)
modelTrend <- lm(formula = CP.decompM$trend[167:487] ~ t)
#model <- lm(CP.decompM$trend ~ poly(t,5))

predT <- predict.lm(modelTrend, newdata = data.frame(t))
plot(CP.decompM$trend[167:487] ~ t, ylab="T(t)", xlab="t",
     type="p", pch=20, main = "Trend Component: Modelled vs Observed")
lines(predT, col="red")






#---------------------------------------------------
#Validation   Recent 4 month were taken as test set 
#---------------------------------------------------


# No trend observed 
#--------------------------------
#Data30 <- data.frame("T" = 1281.598049 + 1.641041 *seq(322, 351, 1) , S=rep(0, 30), e=rep(0,30),
#                       row.names = seq(1,30,1))
#Data30  #This is a new data set created with predicted components 

1970 = x*Data30$S[1] * Data30$e[1]
x= 1970/(Data30$S[1] * Data30$e[1])
Data30 <- data.frame(T=rep(x,30),S=rep(0, 30), e=rep(0,30),row.names = seq(1,30,1))








#Sesonal component 
CP.decompM$seasonal
newdata<-as.data.frame(unique(CP.decompM$seasonal))
newdata
Data30$S<-newdata$`unique(CP.decompM$seasonal)`[1:30]



plot(density(CP.decompM$random[163:486]),
     main="Random Error")
Data30$e <- 1
Data30


#--------------------------------------
sd_error <- sd(CP.decompM$random[163:486])

Data30$R <- Data30$T * Data30$S * Data30$e                  #Realistic Estimation
Data30$O <- Data30$T * Data30$S * (Data30$e+1.95*sd_error)  #Optimistic Estimation
Data30$P <- Data30$T * Data30$S * (Data30$e-1.95*sd_error)  #Pessimistic Estimation
Data30
#------------------------------------------







#--------------------------------------
#  649 Rows
xr = c(1,120)
limit_data<-CP.decompM$x[560:649]
plot(limit_data, xlim=xr, ylab = "stock (Rs)", xlab = "days")
lines(data.frame(CP.decompM$x[560:649]))

lines(Data30$R, x=seq(89,118,1), col="black")
lines(Data30$O, x=seq(89,118,1), col="green")

lines(Data30$P, x=seq(89,118,1), col="red")

