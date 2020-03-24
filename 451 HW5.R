data <- read.table("salmon.dat")
data <- drop(data[2:41,])
str(data)

mydata <- matrix(rep(0,120),40,3)
for(i in 1:3){
  mydata[,i] <- data[,i]
}

for(i in 1:40){
  mydata[i,2] <- 1/mydata[i,2]
  mydata[i,3] <- 1/mydata[i,3]
}
colnames(mydata) <- c("year","R","S")
mydata <- data.frame(mydata)
fit <- lm(R~S,data = mydata)
summary(fit)
#Call:
#lm(formula = R ~ S, data = mydata)

#Residuals:
#      Min        1Q    Median        3Q       Max 
#-0.045808 -0.015937 -0.009741  0.002916  0.143054 

#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.012515   0.006546   1.912   0.0635 .  
#S           1.033294   0.032527  31.767   <2e-16 ***
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.03507 on 38 degrees of freedom
#Multiple R-squared:  0.9637,	Adjusted R-squared:  0.9628 
#F-statistic:  1009 on 1 and 38 DF,  p-value: < 2.2e-16

a <- matrix(c(1,1,-1.033294,-1),2,2)
b <- matrix(c(0.012515,0),2,1)
solve(a,b)
#     [,1]
#[1,] -0.3758936
#[2,] -0.3758936

x <- rep(0, 40)
y <- rep(0, 40)
for(i in 1:40) {
  x[i] <- mydata[i,1]
  y[i] <- mydata[i,2]
}
beta0 <- fit$coef[1]       # estimate for beta0
beta1 <- fit$coef[2]       # estimate for beta1
theta <- beta1/beta0        # estimate of beta1/beta1
theta

# bootstrapping the residuals
set.seed(101)
B <- 10000
n <- length(x)              # size of original sample
yfitted <- lm(y~x)$fitted   # fitted y
residual <- y - yfitted     # fitted residuals
thetastar0 <- rep(0, B)     # bootstrapped estimates for theta1/theta0
Rfstar0 <- rep(0, B)        # bootstrapped (theta^hat-theta)/sqrt(V(F^hat))
for(i in 1:B) {             # loop begins
  temp <- sample(residual, size=n, replace=T);  # bootstrapped residuals
  tempy <- yfitted + temp;  # boostrapped y
  temp <- lm(tempy~x);
  temp0 <- temp$coef[1]     # estimate for beta0
  temp1 <- temp$coef[2]     # estimate for beta1
  thetastar0[i] <- temp1/temp0;
  temp <- vcov(temp)        # variance-covariance matrix of (beta0, beta1), for Example 9.7
  temp2 <- (temp1/temp0)^2*(temp[2,2]/temp1^2+temp[1,1]/temp0^2-2*temp[1,2]/(temp0*temp1));
  Rfstar0[i] <- (temp1/temp0-theta)/sqrt(temp2);
}
summary(thetastar0)
hist(thetastar0, xlab="Bootstrap estimates of theta=beta1/beta0", nclass=40)

quantile(thetastar0, c(0.025, 0.975))

# bootstrapping the cases
n <- length(x)              # size of original sample
thetastar <- rep(0, B)      # bootstrapped estimates for theta=theta1/theta0
Rfstar <- rep(0, B)         # bootstrapped (theta^hat-theta)/sqrt(V(F^hat))
for(i in 1:B) {             # loop begins
  xyindex <- sample(seq(1:n), size=n, replace=T);
  tempx <- x[xyindex];
  tempy <- y[xyindex];
  temp <- lm(tempy~tempx);
  temp0 <- temp$coef[1]     # estimate for beta0
  temp1 <- temp$coef[2]     # estimate for beta1
  thetastar[i] <- temp1/temp0;  # estimate for theta
  temp <- vcov(temp)        # variance-covariance matrix of (beta0, beta1), for Example 9.7
  temp2 <- (temp1/temp0)^2*(temp[2,2]/temp1^2+temp[1,1]/temp0^2-2*temp[1,2]/(temp0*temp1));
  Rfstar[i] <- (temp1/temp0-theta)/sqrt(temp2);
}
summary(thetastar)
se <- sd(thetastar)/sqrt(length(thetastar))
quantile(thetastar, c(0.025, 0.975))
hist(thetastar, xlab="Bootstrap estimates of theta=beta1/beta0", nclass=40)

#B)
temp <- read.table("salmon.dat", header=T)
data.size <- dim(mydata)[1]
x <- rep(0, data.size)
y <- rep(0, data.size)
for(i in 1:data.size) {x[i] <- temp[i,1]; y[i] <- temp[i,2];}
# use linear regression to fit the model Y = beta0 + beta1*x + epsilon
temp <- lm(y~x)             # linear regression of y on x
summary(temp)
beta0 <- temp$coef[1]       # estimate for beta0
beta1 <- temp$coef[2]       # estimate for beta1
theta <- beta1/beta0        # estimate of beta1/beta1
theta
sd2 <- sqrt(sum(temp$residuals^2)/temp$df.residual)   # estimate for standard error
sd2 
yfit <- temp$fitted.values

# generate new dataset
N=1000   # number of simuated data
set.seed(101)
ymat=matrix(0,N,data.size)  # use the fitted model as true model and generate new y
for(i in 1:N) ymat[i,]=yfit+rnorm(data.size,0,sd2);
theta1vec<-rep(0,N)
ttemp=proc.time()
for(i in 1:N) {
  temp <- lm(ymat[i,]~x)             # linear regression of y on x
  theta1vec[i] <- temp$coef[2]/temp$coef[1]        # estimate of beta1/beta1
}
proc.time()-ttemp

mean(theta1vec)-theta

temp <- vcov(temp)          # variance-covariance matrix of (beta0, beta1)
vfhat <- (beta1/beta0)^2*(temp[2,2]/beta1^2+temp[1,1]/beta0^2-2*temp[1,2]/(beta0*beta1))
vfhat


bias <- mean(thetastar) - theta      # mean value of thetahat^* - thetahat
bias   
theta - bias                         # bias-corrected bootstrap estimate


#2
#a)
temp <- read.table("cancersurvival.dat", header = T)
data.size <- dim(temp)[1]
x <- rep(0, data.size)
y <- rep(0, data.size)
for(i in 1:data.size) {x[i] <- temp[i,1]; y[i] <- temp[i,2];}
# use linear regression to fit the model Y = beta0 + beta1*x + epsilon
temp <- lm(y~x)             # linear regression of y on x
summary(temp)
beta0 <- temp$coef[1]       # estimate for beta0
beta1 <- temp$coef[2]       # estimate for beta1
theta <- beta1/beta0        # estimate of beta1/beta1
theta
sd2 <- sqrt(sum(temp$residuals^2)/temp$df.residual)   # estimate for standard error
sd2
yfit <- temp$fitted.values
n <- length(x)
B <- 10000
n <- length(x)              # size of original sample
thetastar <- rep(0, B)      # bootstrapped estimates for theta=theta1/theta0
Rfstar <- rep(0, B)         # bootstrapped (theta^hat-theta)/sqrt(V(F^hat))
for(i in 1:B) {             # loop begins
  xyindex <- sample(seq(1:n), size=n, replace=T);
  tempx <- x[xyindex];
  tempy <- y[xyindex];
  temp <- lm(tempy~tempx);
  temp0 <- temp$coef[1]     # estimate for beta0
  temp1 <- temp$coef[2]     # estimate for beta1
  thetastar[i] <- temp1/temp0;  # estimate for theta
  temp <- vcov(temp)        # variance-covariance matrix of (beta0, beta1), for Example 9.7
  temp2 <- (temp1/temp0)^2*(temp[2,2]/temp1^2+temp[1,1]/temp0^2-2*temp[1,2]/(temp0*temp1));
  Rfstar[i] <- (temp1/temp0-theta)/sqrt(temp2);
}

# bootstrapping the residuals
set.seed(2)
B <- 10000
n <- length(x)              # size of original sample
yfitted <- lm(y~x)$fitted   # fitted y
residual <- y - yfitted     # fitted residuals
thetastar0 <- rep(0, B)     # bootstrapped estimates for theta1/theta0
Rfstar0 <- rep(0, B)        # bootstrapped (theta^hat-theta)/sqrt(V(F^hat))
for(i in 1:B) {             # loop begins
  temp <- sample(residual, size=n, replace=T);  # bootstrapped residuals
  tempy <- yfitted + temp;  # boostrapped y
  temp <- lm(tempy~x);
  temp0 <- temp$coef[1]     # estimate for beta0
  temp1 <- temp$coef[2]     # estimate for beta1
  thetastar0[i] <- temp1/temp0;
  temp <- vcov(temp)        # variance-covariance matrix of (beta0, beta1), for Example 9.7
  temp2 <- (temp1/temp0)^2*(temp[2,2]/temp1^2+temp[1,1]/temp0^2-2*temp[1,2]/(temp0*temp1));
  Rfstar0[i] <- (temp1/temp0-theta)/sqrt(temp2);
}

thetai <- rep(0, n)         # thetahat_(-i), thetahat computed omitting the ith observation
for(i in 1:n) {
  tempx <- x[-i];
  tempy <- y[-i];
  temp <- lm(tempy~tempx)$coef;
  thetai[i] <- temp[2]/temp[1];
}
psii <- mean(thetai) - thetai      
a <- sum(psii^3)/(6*(sum(psii^2))^(3/2))     # a, (9.12) on page 296
a 
b <- qnorm( sum(thetastar <= theta)/B )      
b 
alpha <- 0.05               # for 95% confidence interval
beta1 <- pnorm(b+(b+qnorm(alpha/2))/(1-a*(b+qnorm(alpha/2))))     
beta1  
beta2 <- pnorm(b+(b+qnorm(1-alpha/2))/(1-a*(b+qnorm(1-alpha/2)))) 
beta2  
quantile(thetastar, c(beta1, beta2)) # a BCa 95% bootstrapped confidence interval

# generate new dataset
N=1000   # number of simuated data
set.seed(323)
ymat=matrix(0,N,data.size)  # use the fitted model as true model and generate new y
for(i in 1:N) ymat[i,]=yfit+rnorm(data.size,0,sd2);
theta1vec<-rep(0,N)
ttemp=proc.time()
for(i in 1:N) {
  temp <- lm(ymat[i,]~x)             # linear regression of y on x
  theta1vec[i] <- temp$coef[2]/temp$coef[1]        # estimate of beta1/beta1
}


B0 <- B1 <- 300
R0star <- rep(0, B0)        # R0(X^*,F^hat) based on bootstrapped samples
R1star <- rep(0, B0)        # R1(X^*, F^hat) based on double bootstrapped samples
tempt <- proc.time()        # record system time
for(i in 1:B0) {            # loop begins for R0(X^*, F^hat)
  xyindex <- sample(seq(1:n), size=n, replace=T);
  tempx <- x[xyindex];
  tempy <- y[xyindex];
  temp <- lm(tempy~tempx)$coef;
  thetai <- temp[2]/temp[1];# theta^*_i
  R0star[i] <- thetai - theta;
  temp1 <- rep(0, B1);      # R0(X^**, F^hat)
  for(j in 1:B1) {          # loop begins for R1(X^*, F^hat)
    xyindex1 <- sample(seq(1:n), size=n, replace=T);
    tempx1 <- tempx[xyindex1];
    tempy1 <- tempy[xyindex1];
    temp <- lm(tempy1~tempx1)$coef;
    temp1[j] <- temp[2]/temp[1] - thetai;
  }
  R1star[i] <- sum(temp1 <= R0star[i])/B1;  # R1^hat(X^*, F^hat), (9.21) on page 300
}
quantile(tempx,c(0.025,0.975))
