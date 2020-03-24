#451 HW2
#Sicheng Tian
#1)a)
#step 1
y <- 1.182*rnorm(100000,0,2*sqrt(3))
qx <- exp(-abs(y^3)/3)
#step 2
weights <- abs(qx/y)
std.weights <- weights/sum(weights)
sir.y <- sample(y, 5000, prob = std.weights, replace=T)
estA <- mean(sir.y^2)
print(estA)

#b)
fq <- function(x) { 
  exp(-abs(x)^3/3);
}
fe <- function(x) { 
  exp(-x^2/2)*1.182;
}
fh <- function(x) { 
  x^2;
}
f <- function(n, LIMIT=Inf) {
  x <- rep(NA,n);
  i <- 0; # index of y
  j <- 0; # index of lambda
  while((i<n)&(j<LIMIT)) {
    y <- rnorm(1);
    u <- runif(1);
    if( u < fq(y)/fe(y) ) {
      i <- i + 1;
      x[i] <- y;
    };
    j <- j + 1;
  }
  list(x=x, acratio=i/j);
}
n <- 10000
temp.b <- f(n)
x <- temp.b$x
estB <- mean(x^2)
print(estB)
#c)
temp.c <- f(n)
x <- temp.c$x
x <- sort(x)
estC <- sum((x[2:n]-x[1:(n-1)])*fh(x[1:(n-1)])*fq(x[1:(n-1)])) / sum((x[2:n]-x[1:(n-1)])*fq(x[1:(n-1)]))
print(estC)

#d
samp.size=5000
n=samp.size
m <- 50
pbes=pces=rep(0, m)
for(i in 1:m) {
  x=f(n)$x;
  pbes[i]=mean(x^2) ;
  x <- sort(x);
  pces[i]=sum((x[2:n]-x[1:(n-1)])*fh(x[1:(n-1)])*fq(x[1:(n-1)])) / sum((x[2:n]-x[1:(n-1)])*fq(x[1:(n-1)]));
}
mean(pbes) 
var(pces) 
mean(pbes)
var(pces) 


#2
#a)
#b)
#SIR
set.seed(1)
x <- rexp(100000,1)
y.a <- abs(rnorm(100000))
y.b <- runif(100000,0,1000)
y.c <- abs(rcauchy(100000,0,1))
wts.a = x/y.a
wts.b = x/y.b
wts.c = x/y.c
std.wts.a <- wts.a/sum(wts.a)
std.wts.b <- wts.b/sum(wts.b)
std.wts.c <- wts.c/sum(wts.c)
sir.x.a <- sample(y.a, 1000, prob = std.wts.a, replace=T)
sir.x.b <- sample(y.b, 1000, prob = std.wts.b, replace=T)
sir.x.c <- sample(y.c, 1000, prob = std.wts.c, replace=T)
mu.a <- mean(sir.x.a)
mu.b <- mean(sir.x.b)
mu.c <- mean(sir.x.c)
print(c(mu.a,mu.b,mu.c))
#MC
fh <- function(x){
  sqrt(x)
}                     #h(x)
x.a <- abs(rnorm(100000))              # simple Monte Carlo sample
x.b <- runif(100000,0,1000)
x.c <- abs(rcauchy(100000,0,1))
y.a <- fh(x.a)
y.b <- fh(x.b)
y.c <- fh(x.c)
MCest.a <- mean(y.a)              #MC estimate
MCest.b <- mean(y.b)
MCest.c <- mean(y.c)
print(c(MCest.a,MCest.b,MCest.c))

#c)
sqrt(var(y.a)/100000)             # estimated std of MC estimator
sqrt(var(y.b)/100000)
sqrt(var(y.c)/100000)
sqrt(var(sir.x.a)/sqrt(1000))
sqrt(var(sir.x.b)/sqrt(1000))
sqrt(var(sir.x.c)/sqrt(1000))