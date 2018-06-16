#Only few things are copied from the below source 
#https://www.r-bloggers.com/implementing-the-gradient-descent-algorithm-in-r/


#Below function explains a batch gradient descent 

x=seq(1:100)
y=50+50*x

gdf <- function(x, y, init_m,init_c,learn_rate, conv_threshold, n, max_iter) {
  plot(x, y, col = "blue", pch = 20)
  m <- init_m
  c <- init_c
  yhat <- m * x + c
  MSE <- sum((y - yhat) ^ 2) / n
  converged = F
  iterations = 0
  
  
  theta1=matrix(nrow=1,ncol=max_iter+1)
  theta0=matrix(nrow=1,ncol=max_iter+1)
  loss=matrix(nrow=1,ncol=max_iter+1)
  
  while(converged == F) {
    ## Implement the gradient descent algorithm
    theta_1 = learn_rate * ((1 / n) * (sum((yhat - y) * x )) )
    theta_0=learn_rate * ((1 / n) * (sum(yhat - y)))
    m_new <- m - learn_rate * ((1 / n) * (sum((yhat - y) * x)))
    c_new <- c - learn_rate * ((1 / n) * (sum(yhat - y)))
    m <- m_new
    c <- c_new
    yhat <- m * x + c
    MSE_new <- sum((y - yhat) ^ 2) / n
    
    iterations = iterations + 1
    
    theta0[1,iterations]= c_new
    theta1[1,iterations]= m_new
    loss[1,iterations]= MSE_new
    
    if(abs(MSE - MSE_new) <= conv_threshold) {
      abline(c, m) 
      converged = T
      
      theta0=as.vector(theta0)
      theta1=as.vector(theta1)
      grads=as.vector(grads)
      loss=as.vector(loss)
      grads_slops=data.frame(theta0,theta1,loss)
      return(grads_slops)
      
#      return(paste("Optimal intercept:", c, "Optimal slope:", m))
    }
#   iterations = iterations + 1
    if(iterations > max_iter) { 
      abline(c, m) 
      converged = T
      
      theta0=as.vector(theta0)
      theta1=as.vector(theta1)
      loss=as.vector(loss)
      grads_slops=data.frame(theta0,theta1,loss)
      return(grads_slops)
      
#      return(paste("Optimal intercept:", c, "Optimal slope:", m))
    }
  }
}

gdf(x,y,10,10,.0000004,.0001,100,200000)

a=gdf(x,y,-100,7,.0000003,.001,100,200000)
b=gdf(x,y,200,7,.0000003,.001,100,200000)

my_data=rbind(a[1:5000,],b[1:5000,])
library(ggplot2)
ggplot(aes(y=loss,x=theta1),data=my_data)+geom_area()



#---------------------------------------------------------------------------------------


#Below function explains a stochastic gradient algorithm 
x=seq(1:700)
y=50*x

sgdf <- function(p, q, init_m,init_c,learn_rate, conv_threshold, n, max_iter) {
  plot(x, y, col = "blue", pch = 20)
  

  #Sampling goes here, each time , I am just considering a 33% of the data   
  default_idx <- sample(length(p), ceiling(length(p) / 3))
  x <-  p[default_idx]
  y <-  q[default_idx]
  
  m <- init_m
#  c <- init_c
  yhat <- m * x 
  MSE <- sum((y - yhat) ^ 2) / n
  converged = F
  iterations = 0
  
  
  theta1=matrix(nrow=1,ncol=max_iter+1)
#  theta0=matrix(nrow=1,ncol=max_iter+1)
  loss=matrix(nrow=1,ncol=max_iter+1)
  
  while(converged == F) {
    
    #Sampling goes here, each time , I am just considering a 33% of the data   
    default_idx <- sample(length(p), ceiling(length(p) / 3))
    x <-  p[default_idx]
    y <-  q[default_idx]
    
    ## Implement the gradient descent algorithm
    theta_1 = learn_rate * ((1 / n) * (sum((yhat - y) * x )) )
 # theta_0=learn_rate * ((1 / n) * (sum(yhat - y)))
    m_new <- m - learn_rate * ((1 / n) * (sum((yhat - y) * x)))
 # c_new <- c - learn_rate * ((1 / n) * (sum(yhat - y)))
    m <- m_new
 #  c <- c_new
    yhat <- m * x 
    MSE_new <- sum((y - yhat) ^ 2) / n
    
    iterations = iterations + 1
    
 #   theta0[1,iterations]= c_new
    theta1[1,iterations]= m_new
    loss[1,iterations]= MSE_new
    
    if(abs(MSE - MSE_new) <= conv_threshold) {
      abline(init_c, m) 
      converged = T
      
 #     theta0=as.vector(theta0)
      theta1=as.vector(theta1)
      grads=as.vector(grads)
      loss=as.vector(loss)
      grads_slops=data.frame(theta1,loss)
      return(grads_slops)
      
      #      return(paste("Optimal intercept:", c, "Optimal slope:", m))
    }
    #   iterations = iterations + 1
    if(iterations > max_iter) { 
      abline(init_c, m) 
      converged = T
      
 #     theta0=as.vector(theta0)
      theta1=as.vector(theta1)
      loss=as.vector(loss)
      grads_slops=data.frame(theta1,loss)
      return(grads_slops)
      
      #      return(paste("Optimal intercept:", c, "Optimal slope:", m))
    }
  }
}


sgdf(x,y,10,0,.0001,.0001,100,200)

a=sgdf(x,y,-100,0,.0000003,.001,100,5000)
b=sgdf(x,y,200,0,.0000003,.001,100,5000)

my_data=rbind(a[1:5000,],b[1:5000,])
library(ggplot2)
ggplot(aes(y=loss,x=theta1),data=my_data)+geom_area()
View(my_data)

#-----------------------------------------------------------------------------------------

attach(mtcars)
plot(disp, mpg, col = "blue", pch = 20)
model <- lm(mpg ~ disp, data = mtcars)
coef(model)


gradientDesc <- function(x, y, learn_rate, conv_threshold, n, max_iter) {
  plot(x, y, col = "blue", pch = 20)
  m <- -10
  c <- 30
  yhat <- m * x + c
  MSE <- sum((y - yhat) ^ 2) / n
  converged = F
  iterations = 0
  while(converged == F) {
    ## Implement the gradient descent algorithm
    temp_1 = learn_rate * ((1 / n) * (sum((yhat - y) * x )) )
    m_new <- m - learn_rate * ((1 / n) * (sum((yhat - y) * x)))
    c_new <- c - learn_rate * ((1 / n) * (sum(yhat - y)))
    m <- m_new
    c <- c_new
    yhat <- m * x + c
    MSE_new <- sum((y - yhat) ^ 2) / n
    
    if(abs(MSE - MSE_new) <= conv_threshold) {
      abline(c, m) 
      converged = T
      return(paste("Optimal intercept:", c, "Optimal slope:", m))
    }
    iterations = iterations + 1
    if(iterations > max_iter) { 
      abline(c, m) 
      converged = T
      return(paste("Optimal intercept:", c, "Optimal slope:", m))
      }
  }
}


gradientDesc(mtcars$disp,mtcars$mpg,.0000003,.00001,nrow(cars),6000000)







