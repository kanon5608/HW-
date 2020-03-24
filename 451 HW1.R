## 1(a
## Truth
integrate(function(x){x/(1+x^2)^2},1,3)
##approximate
x <- runif(100000,1,3)
(3-1)*mean(x/(1+x^2)^2)

##1(b
##Truth
integrate(function(x){x^2*exp(-x^2)},-Inf,Inf)
##approximate
x <- rnorm(100000,0,sqrt(2)/2)
sqrt(2*pi)*sqrt(2)/2*mean(x^2)

##1(c
library("pracma")
##Truth
I.c <- function(x,y) {
  abs(x-y)
}
integral2(I.c,-1,1,-1,1)
##approximate
x <- runif(100000)
y <- runif(100000)
8*mean(abs(x-y))

##2 ##question!!!!
X <- rlnorm(100000)
error <- rnorm(100000)
Y <- exp(9 + 3*log(X) + error)
mean(Y/X)

##3
f <- function(x) { exp(-abs(x)^3/3) }; # -Inf,Inf

fe <- function(n, LIMIT=Inf) { 
  x <- rep(NA,n);
  i <- 0;                      # index of x
  j <- 0;                      # index of y, candidate of x
  while((i<n)&(j<LIMIT)) {
    y <- 1.182*sqrt(2*pi)*rnorm(1);
    u <- runif(1);
    if(u <= f(y)) {
      i <- i + 1;
      x[i] <- y;
    };
    j <- j + 1;
  };
  list(x=x, ratio=i/j);
}

# plot sampling results
temp <- fe(100000)    
temp$ratio                   
par(mfrow=c(1,1))
hist(temp$x, xlim=c(-4,4), freq=F, nclass=50, xlab="x", )
curve(f, -1, 1, add=T)