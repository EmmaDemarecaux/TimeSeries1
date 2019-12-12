## This empties the work space.
rm(list=ls())
library(ggplot2)
## change directory
## setwd("/path/to/work_dir")
setwd("~/DTU/Time_series1")


## reading data,regarding the first lines in the file as names:
data <- read.table(file="A1_diesel.txt", sep="\t", header=TRUE)
head(data)
names(data) <- c("Year","Diesel")

#Dimension of the dataset
N <- length(data$Year)

#I create a new dataset without the last three years
data_estim <- data.frame(data$Year[1:48],data$Diesel[1:48])
names(data_estim) <- c('Year','Diesel')
data_comp <- data.frame(data$Year[49:51],data$Diesel[49:51])
names(data_comp) <- c('Year','Diesel')

#Legnth of the new dataset
N_estim <- length(data_estim$Year)

################################Question 1#####################
par(mfrow=c(1,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))
plot(data$Year, data$Diesel, xlab='Year',ylab="Diesel [Tonnes]",type='o',pch=20,
     main='Consumption of diesel in tonnes from 1966 to 2016')

################################Question 2#####################

#Computation of the mean and the standard deviation
mean <- mean(data$Diesel)
sd <- sd(data$Diesel)

#Constant mean model
eps <- rnorm(N, mean = 0, sd = sd)
CMM <- mean + eps
  
par(mfrow=c(1,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))
plot(data$Year, data$Diesel, xlab='Year',ylab="Diesel [Tonnes]",type='o',pch=20,
     main='Consumption of diesel in tonnes from 1966 to 2016', ylim=c(min(CMM),max(CMM)))
lines(data$Year, CMM, type='l', col="red",lty=2)

legend("topleft", legend=c("Consumption of Diesel", "Constant mean model"),
       col=c("black", "red"), lty=1:2, cex=0.8)

#histogram
breaks <- 25

h <- hist(data$Diesel,col="white",breaks = breaks,
          main = "Histogram with Normal Curve",
          xlab='Consumption of diesel in tonnes')

xfit <- seq(min(data$Diesel),max(data$Diesel),length=10000) 
yfit <- dnorm(xfit,mean=mean,sd=sd)
yfit <- yfit*diff(h$mids[1:2])*N
lines(xfit, yfit, col="black", lwd=2)
segments(mean,
         dnorm(mean,mean=mean,sd=sd)*diff(h$mids[1:2])*N,
         mean,0,lwd=2,col="red",lty=2)
legend('topright',legend=c("Normal distributed repartition",
                           "Mean value"),col=c("black",'red'),
       lty =c(1,2),lwd=c(2,2),cex = 0.8)

################################Question 3#####################
##########################General linear model################

#GLM (simple linear regression) on the new dataset using the function lm()
GLM <- lm(data_estim$Diesel ~  data_estim$Year)

#Theta_hat
coefficients(GLM)
## No need to remove least significant term because Pr(>|t|) << 1
summary(GLM)

par(mfrow=c(1,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))
plot(data_estim$Year, data_estim$Diesel, 
     main="General linear model (simple linear regression)",type='o',pch=20,
     ylim=c(GLM$fitted.values[1],max(data_estim$Diesel)))
lines(data_estim$Year, GLM$fitted.values, xlab='Year', ylab='Diesel [Tonnes]',
      type='l', col="red",lty=1, lwd=2)

legend("topleft", legend=c("Consumption of Diesel", "GLM"),
       col=c("black", "red"), lty=c(1,1), cex=0.8)

#I estimate sigma - 2 parameters estimated
sigma_hat <- sqrt(sum(GLM$residuals^2)/(N_estim-2))

#residuals:
par(mfrow=c(1,2))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))
plot(data_estim$Year, data_estim$Diesel-GLM$fitted.values, 
     main="Residuals GLM",type='o',pch=20,
     xlab='Year', ylab='Diesel [Tonnes]')
lines(data_estim$Year, rep(0,N_estim),type='l', col="red",lty=2, lwd=2)

legend("topleft", legend=c("Residuals", "Supposed mean value (0)"),
       col=c("black", "red"), lty=c(1,1), cex=0.5)
## Checking distribution of residuals:
h <- hist(GLM$residuals, probability=T, col="white",breaks = breaks,
     main = "Histogram of residuals (GLM order 1)",xlab='Residuals',
     lwd=2,lty=1)
curve(dnorm(x,mean = 0, sd = sigma_hat),add=TRUE,col='black', lwd=1)
legend('topleft',legend=c("Normal distributed repartition"),col=c("black"),
       lty =1,lwd=2,cex = 0.5)

#mean 0

# Predict 3 step ahead, i.e. for time 49:51 i.e. from year 2014:2016
X <- cbind(1,data_estim$Year)
predict.intervals <- numeric(3)

#Prediction interval95% - 2 parameters estimated
for(i in 1:3){
  predict.intervals[i] <- qt(0.975, N_estim-2)*sigma_hat* 
    sqrt( (1+t(c(1,2013+i)) %*% solve(t(X)%*%X) %*% c(1,2013+i)) )
}

Prediction <- data.frame(pred = GLM$coefficients[1]*1+
                           GLM$coefficients[2]*c(2014,2015,2016))
 
Prediction$lower <- Prediction$pred - predict.intervals
Prediction$upper <- Prediction$pred + predict.intervals
head(Prediction)

par(mfrow=c(1,2))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))
plot(data_estim$Year, data_estim$Diesel, xlab='Year',ylab='Diesel',
     main="Prediction - GLM (order 1)", xlim=c(1966, 2016),
     ylim=c(min(data_estim$Diesel),max(data_comp$Diesel)))
lines(data_estim$Year, GLM$fitted.values, type="l", lwd=2)
lines(2014:2016, Prediction$pred, col='red',lwd=2)
lines(2014:2016, Prediction$lower, col='green',lwd=2)
lines(2014:2016, Prediction$upper, col='green',lwd=2)
lines(data_comp$Year,data_comp$Diesel,type='b',col='blue',lwd=2)

#Let's try to formulate a GLM model with three parameters manually

X <- cbind(1,data_estim$Year-2013,I(data_estim$Year-2013)^2)
theta <- solve(t(X)%*%X, t(X)%*% data_estim$Diesel) 
theta
#I estimate the new data
Diesel_estim <- X%*%theta

par(mfrow=c(1,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))
plot(data_estim$Year, data_estim$Diesel, xlab = 'Year', ylab = 'Diesel [Tonnes]',
     main="General linear model (order 2)",type='o',pch=20)
lines(data_estim$Year, Diesel_estim, type='l', col="red",lty=1, lwd=2)
legend("topleft", legend=c("Consumption of Diesel", "GLM 2"),
       col=c("black", "red"), lty=c(1,1), cex=0.8)

#I estimate the new sigma_hat - 3 parameters estimated
res <- data_estim$Diesel-Diesel_estim
sigma_hat <- sqrt( sum((res)^2)/(N_estim-3) )

#residuals:
par(mfrow=c(1,2))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))
plot(data_estim$Year, res, 
     main="Residuals GLM 2",type='o',pch=20,
     xlab='Year', ylab='Diesel [Tonnes]')
lines(data_estim$Year, rep(0,N_estim),type='l', col="red",lty=2, lwd=2)

legend("topleft", legend=c("Residuals", "Supposed mean value (0)"),
       col=c("black", "red"), lty=c(1,1), cex=0.5)
## Checking distribution of residuals:
h <- hist(res, probability=T, col="white",breaks = breaks,
          main = "Histogram of residuals (GLM 2)",xlab='Residuals',
          lwd=2,lty=1)
curve(dnorm(x,mean = 0, sd = sigma_hat),add=TRUE,col='black', lwd=1)
legend('topleft',legend=c("Normal distributed repartition"),col=c("black"),
       lty =1,lwd=2,cex = 0.5)
#mean zero

# Predict 3 step ahead, i.e. for time 49:51 i.e. for year 2014:2016
predict.intervals <- numeric(3)

#Prediction interval 95%
for(i in 1:3){
  predict.intervals[i] <- qt(0.975, N_estim-3) * sigma_hat* 
    sqrt( (1+t(c(1,i,i^2)) %*% solve(t(X)%*%X) %*% c(1,i,i^2)) )
}

X_pred = cbind(1,c(1,2,3),I(c(1,2,3)^2))

Prediction <- data.frame(pred = X_pred%*%theta)
# Prediction intervals 
Prediction$lower <- Prediction$pred - predict.intervals
Prediction$upper <- Prediction$pred + predict.intervals
head(Prediction)

plot(data_estim$Year, data_estim$Diesel, xlab='Year',ylab='Diesel',
     main="Prediction - GLM (order 2)", xlim=c(1966, 2016),
     ylim=c(min(data_estim$Diesel),
            max(max(data_comp$Diesel),max(Prediction$upper))))
lines(data_estim$Year, Diesel_estim, type="l", lwd=2)
lines(2014:2016, Prediction$pred, col='red',lwd=2)
lines(2014:2016, Prediction$lower, col='green',lwd=2)
lines(2014:2016, Prediction$upper, col='green',lwd=2)
lines(data_comp$Year,data_comp$Diesel,type='b',col='blue',lwd=2)

################################Question 4#####################
#Exponential Smoothing
lambda <- 0.8

mu <- rep(0,N_estim)
mu[1] <-data$Diesel[1]
for (i in 2:(N_estim))
{
  mu[i]<-(1-lambda)*data$Diesel[i]+lambda*mu[i-1]
}

#Prediction : Y(N+l) = mu(N)
Y_SexpS <- c(data$Diesel[1],mu[1:N_estim],mu[48],mu[48])

par(mfrow=c(1,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))
plot(data$Year, data$Diesel, xlab = 'Year', ylab = 'Diesel [Tonnes]',
     main="Simple exponential smoothing 0.8",type='o',pch=20,lwd=1.5)
lines(data$Year[1:48], Y_SexpS[1:48], type='o',pch=20, col="red",lty=1,lwd=1.5)
lines(data$Year[49:51], Y_SexpS[49:51], type='o', col="red",lty=1,lwd=1.5,pch=3)
legend("topleft", legend=c("Consumption of Diesel", "Estimation","Prediction"),
       col=c("black", "red","red"),lty=c(1,1,1),lwd=c(1.5,1.5,1.5),
       pch=c(20,20,3), cex=0.8)

 
################################Question 5#####################
#Local linear trend model
lambda<-0.8 

#Burning period
burn<-5

#Defining matrix
f<- function(j) rbind(1,j)
F<-rbind(c(0,0),c(0,0))
h<-rbind(0,0)
L<-rbind(c(1,0),c(1,1))

#Initiating the prediction during the burning period
Diesel_est<-rbind(data$Diesel[1],data$Diesel[2],data$Diesel[3],data$Diesel[4])

for (j in 0:(burn-1)) 
{
  F<- F + lambda^(j)*f(-j)%*%t(f(-j)) 
  h<- h + lambda^(j)*f(-j)%*%t(data$Diesel[burn-j]) 
}

theta<-solve(F,h) 
#F5, h5, theta5
weight_sigmag <- 1/( 1 + t(f(1))%*%solve(F)%*%f(1))

Diesel_est<-rbind(Diesel_est,t(f(0))%*%theta) 
Diesel_pred<-t(f(1))%*%theta

#looping on the new observations 
for (j in burn:(N_estim-2)) 
{
  F <- F + lambda^(j)*f(-j)%*%t(f(-j)) 
  h <- lambda*solve(L)%*%h + f(0)*data$Diesel[j+1] 
  theta<- solve(F,h)

  Diesel_est<-rbind(Diesel_est,t(f(0))%*%theta ) 
  Diesel_pred<-rbind(Diesel_pred,t(f(1))%*%theta)
  weight_sigmag <- rbind(weight_sigmag,1/(1 + t(f(1))%*%solve(F)%*%f(1)))
}
#F(N_estim -1)
#Diesel_perd -> YN_estim|N_estim-1
j <- N_estim-1
F <- F + lambda^(j)*f(-j)%*%t(f(-j)) 
h <- lambda*solve(L)%*%h + f(0)*data$Diesel[j+1] 
theta<- solve(F,h)

Diesel_est<-rbind(Diesel_est,t(f(0))%*%theta ) 

#Standard deviation of the residual LLM
T <- (1-lambda^(N_estim))/(1-lambda) 
#sigmal2 <- 1/(T-2)*sum(lambda^(seq(N_estim-1,0,-1))
#                              *(data_estim$Diesel-Diesel_est)^2)

#In this case, we used a different theta for each estimation
#Hence, the weight is already given to the error
#there is no need to multiply the error with ambda^(seq(N_estim-1,0,-1)
sigmal2 <- 1/(T-2)*sum((data_estim$Diesel-Diesel_est)^2)

sigmag2 <- 1/(N_estim-burn)*
  sum((data$Diesel[6:N_estim]-Diesel_pred)^2*weight_sigmag)

X_pred <- cbind(1,c(1,2,3))
Y_pred <- X_pred%*%theta


# Predict 3 step ahead, i.e. for time 49:51 i.e. for year 2014:2016
predict.intervals <- data.frame(loc=numeric(3),glob=numeric(3))

#Prediction interval 95%
for(i in 1:3){
  predict.intervals$loc[i] <- qt(0.975, T-2) * sqrt(sigmal2)* 
    sqrt(1 + t(f(i))%*%solve(F)%*%f(i))
  predict.intervals$glob[i] <- qt(0.975, N_estim-burn) * sqrt(sigmag2)* 
    sqrt(1 + t(f(i))%*%solve(F)%*%f(i))
}

Pred <- data.frame(pred = Y_pred)
# Prediction intervals 
Pred$lower_loc <- Pred$pred - predict.intervals$loc
Pred$upper_loc <- Pred$pred + predict.intervals$loc
Pred$lower_glob <- Pred$pred - predict.intervals$glob
Pred$upper_glob <- Pred$pred + predict.intervals$glob

Pred <- data.frame(Year=c(2014,2015,2016),Pred)

par(mfrow=c(1,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))
plot(data$Year, data$Diesel, xlab='Year', ylab='Diesel [Tonnes]',
     main="Global and local linear trend model",type='o',pch=20,lwd=1.5,
     cex = 0.5,ylim = c(min(Diesel_pred),max(Pred$upper_loc)))
lines(data$Year[(burn+1):48], Diesel_pred, pch = 20, cex = 0.5, 
      col = 'red', type = 'o')
lines(data$Year[49:51], Y_pred, pch = 3, cex = 0.5, col = 'red', type = 'l')
lines(data$Year[49:51], Pred$lower_glob, pch = 3, cex = 0.5, 
      col = 'green', type = 'l',lty=5) 
lines(data$Year[49:51], Pred$upper_glob, pch = 3, cex = 0.5, 
      col = 'green', type = 'l',lty=5) 
lines(data$Year[49:51], Pred$lower_loc, pch = 3, cex = 0.5, 
      col = 'blue', type = 'l',lty=5) 
lines(data$Year[49:51], Pred$upper_loc, pch = 3, cex = 0.5, 
      col = 'blue', type = 'l',lty=5)
legend('topleft',legend = c("Observations", "One-step prediction", "Prediction",
                            "95% global estimator", '95% local estimator'), 
       col = c("black","red","red",'green','blue'),lty = c(1,1,1,2,2), 
       pch = c(16,16,NA,NA,NA),cex=0.7, bty = 'n')

################################Question 6#####################
lambdas <- seq(0.001,0.999,0.001)

#sum of squared one-step prediction errors
SSE <- c()
sigmals <- c()
burn <- 5

#looping on the lambda
for (lambda in lambdas)
{
  #Defining matrix
  f<- function(j) rbind(1,j)
  F<-rbind(c(0,0),c(0,0))
  h<-rbind(0,0)
  L<-rbind(c(1,0),c(1,1))

  for (j in 0:(burn-1)) 
  {
    F<- F + lambda^(j)*f(-j)%*%t(f(-j)) 
    h<- h + lambda^(j)*f(-j)%*%t(data$Diesel[burn-j]) 
  }
  
  theta<-solve(F,h) 
  #F5, h5, theta5

  Diesel_pred<-t(f(1))%*%theta
  #Y6|5
  
  #looping on the new observations 
  for (j in burn:(N_estim-2)) 
  {
    F <- F + lambda^(j)*f(-j)%*%t(f(-j)) 
    h <- lambda*solve(L)%*%h + f(0)*data$Diesel[j+1] 
    theta<- solve(F,h)
    Diesel_pred<-rbind(Diesel_pred,t(f(1))%*%theta)
  }
  #F(N_estim -1)
  #Diesel_perd -> YN_estim|N_estim-1

  error <- sum((data$Diesel[6:N_estim]-Diesel_pred)^2)
  SSE <- rbind(SSE,error)
}

plot(lambdas,SSE,main='Sum of squared one-step prediction errors',type = 'l')

lambda_min <- lambdas[which.min(SSE)]
#0.666

#Local linear trend model
lambda<-lambda_min

#Burning period
burn<-5

#Defining matrix
f<- function(j) rbind(1,j)
F<-rbind(c(0,0),c(0,0))
h<-rbind(0,0)
L<-rbind(c(1,0),c(1,1))

#Initiating the prediction during the burning period
Diesel_est<-rbind(data$Diesel[1],data$Diesel[2],data$Diesel[3],data$Diesel[4])

for (j in 0:(burn-1)) 
{
  F<- F + lambda^(j)*f(-j)%*%t(f(-j)) 
  h<- h + lambda^(j)*f(-j)%*%t(data$Diesel[burn-j]) 
}

theta<-solve(F,h) 
#F5, h5, theta5
weight_sigmag <- 1/( 1 + t(f(1))%*%solve(F)%*%f(1))

Diesel_est<-rbind(Diesel_est,t(f(0))%*%theta) 
Diesel_pred<-t(f(1))%*%theta

#looping on the new observations 
for (j in burn:(N_estim-2)) 
{
  F <- F + lambda^(j)*f(-j)%*%t(f(-j)) 
  h <- lambda*solve(L)%*%h + f(0)*data$Diesel[j+1] 
  theta<- solve(F,h)
  
  Diesel_est<-rbind(Diesel_est,t(f(0))%*%theta ) 
  Diesel_pred<-rbind(Diesel_pred,t(f(1))%*%theta)
  weight_sigmag <- rbind(weight_sigmag,1/(1 + t(f(1))%*%solve(F)%*%f(1)))
}
#F(N_estim -1)
#Diesel_perd -> YN_estim|N_estim-1
j <- N_estim-1
F <- F + lambda^(j)*f(-j)%*%t(f(-j)) 
h <- lambda*solve(L)%*%h + f(0)*data$Diesel[j+1] 
theta<- solve(F,h)

Diesel_est<-rbind(Diesel_est,t(f(0))%*%theta ) 

#Standard deviation of the residual LLM
T <- (1-lambda^(N_estim))/(1-lambda) 
#sigmal2 <- 1/(T-2)*sum(lambda^(seq(N_estim-1,0,-1))
#                              *(data_estim$Diesel-Diesel_est)^2)

#In this case, we used a different theta for each estimation
#Hence, the weight is already given to the error
#there is no need to multiply the error with ambda^(seq(N_estim-1,0,-1)
sigmal2 <- 1/(T-2)*sum((data_estim$Diesel-Diesel_est)^2)

sigmag2 <- 1/(N_estim-burn)*
  sum((data$Diesel[6:N_estim]-Diesel_pred)^2*weight_sigmag)

X_pred = cbind(1,c(1,2,3))
Y_pred <- X_pred%*%theta

# Predict 3 step ahead, i.e. for time 49:51 i.e. for year 2014:2016
predict.intervals <- data.frame(loc=numeric(3),glob=numeric(3))

#Prediction interval 95%
for(i in 1:3){
  predict.intervals$loc[i] <- qt(0.975, T-2) * sqrt(sigmal2)* 
    sqrt(1 + t(f(i))%*%solve(F)%*%f(i))
  predict.intervals$glob[i] <- qt(0.975, N_estim-burn) * sqrt(sigmag2)* 
    sqrt(1 + t(f(i))%*%solve(F)%*%f(i))
}

Pred <- data.frame(pred = Y_pred)
# Prediction intervals 
Pred$lower_loc <- Pred$pred - predict.intervals$loc
Pred$upper_loc <- Pred$pred + predict.intervals$loc
Pred$lower_glob <- Pred$pred - predict.intervals$glob
Pred$upper_glob <- Pred$pred + predict.intervals$glob

Pred <- data.frame(Year=c(2014,2015,2016),Pred)

par(mfrow=c(1,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))
plot(data$Year, data$Diesel, xlab='Year', ylab='Diesel [Tonnes]',
     main="Global and local linear trend model",type='o',pch=20,lwd=1.5,
     cex = 0.5,ylim = c(min(Diesel_pred),max(data$Diesel)))
lines(data$Year[(burn+1):48], Diesel_pred, pch = 20, cex = 0.5, 
      col = 'red', type = 'o')
lines(data$Year[49:51], Y_pred, pch = 20, cex = 0.5, col = 'red', type = 'l')
lines(data$Year[49:51], Pred$lower_glob, pch = 3, cex = 0.5, 
      col = 'green', type = 'l',lty=5) 
lines(data$Year[49:51], Pred$upper_glob, pch = 3, cex = 0.5, 
      col = 'green', type = 'l',lty=5) 
lines(data$Year[49:51], Pred$lower_loc, pch = 3, cex = 0.5, 
      col = 'blue', type = 'l',lty=5) 
lines(data$Year[49:51], Pred$upper_loc, pch = 3, cex = 0.5, 
      col = 'blue', type = 'l',lty=5)
legend('topleft',legend = c("Observations", "One-step prediction", "Prediction",
                            "95% global estimator", '95% local estimator'), 
       col = c("black","red","red",'green','blue'),lty = c(1,1,1,2,2), 
       pch = c(16,16,NA,NA,NA),cex=0.7, bty = 'n')

#I estimate the new sigma_hat - 3 parameters estimated
res <- data$Diesel[6:N_estim]-Diesel_pred

#residuals:
par(mfrow=c(1,2))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))
plot(data$Year[6:N_estim], res, 
     main="Residuals local linear trend model",type='o',pch=20,
     xlab='Year', ylab='Diesel [Tonnes]')
lines(data_estim$Year, rep(0,N_estim),type='l', col="red",lty=2, lwd=2)

legend("topleft", legend=c("Residuals", "Supposed mean value (0)"),
       col=c("black", "red"), lty=c(1,1), cex=0.5)
## Checking distribution of residuals:
h <- hist(res, probability=T, col="white",breaks = breaks,
          main = "Histogram of residuals (LLTM)",xlab='Residuals',
          lwd=2,lty=1)
curve(dnorm(x,mean = 0, sd = sigma_hat),add=TRUE,col='black', lwd=1)
legend('topleft',legend=c("Normal distributed repartition"),col=c("black"),
       lty =1,lwd=2,cex = 0.5)
#mean zero