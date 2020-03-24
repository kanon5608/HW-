## Source:   chap6.txt in
## http://www.stat.colostate.edu/computationalstatistics/datasets/computationalstatistics.zip

#________________________________________________________________
#________________________________________________________________
# IMPORTANCE SAMPLING
#
# Key idea:  Let X~exponential(1).  Use importance sampling to find
# E(sqrt(x)).   Compare the performance of IS using various envelopes.
# Some code is commented out for use in exercises in 6.1 below.

# Importance sampling (weights are not standardized)
samp.size=10000

## Envelopes
##Case 1: abs(Normal(0,1))
set.seed(1)
x=abs(rnorm(samp.size))
wts=dexp(x)/(dnorm(x)/.5)

# Importance sampling estimate of the mean
mu.is = (1/samp.size)*sum(sqrt(x)*wts)
mu.is

#SE mu.is
semu.is <- (1/samp.size)*var(dexp(x)* wts)
print(semu.is)

#SE mu.is standard
semu.is <- (1/samp.size)*var(dexp(x)* wts) + mu.is^2*var(wts) - 2*mu.is*cov(dexp(x)*wts,wts)
print(semu.is)

#variance of the weights and effective sample size
var(wts)
samp.size/(1+var(wts)) #ESS

#Importance sampling with standardized weights, estimated mean
mu.sis = sum(sqrt(x)*wts)/sum(wts)
mu.sis


##Case 2: uniform(0,1000)
set.seed(1)
x=runif(samp.size,0,1000)
wts=dexp(x)/(1/1000)

# Importance sampling estimate of the mean
mu.is = (1/samp.size)*sum(sqrt(x)*wts)
mu.is
#SE mu.is
semu.is <- (1/samp.size)*var(dexp(x)* wts)
print(semu.is)

#SE mu.is standard
semu.is <- (1/samp.size)*var(dexp(x)* wts) + mu.is^2*var(wts) - 2*mu.is*cov(dexp(x)*wts,wts)
print(semu.is)

#variance of the weights and effective sample size
var(wts)
samp.size/(1+var(wts)) #ESS

#Importance sampling with standardized weights, estimated mean
mu.sis = sum(sqrt(x)*wts)/sum(wts)
mu.sis


##Case 3: abs(Cauchy(0,1))
set.seed(1)
x=abs(rcauchy(samp.size))
wts=dexp(x)/(dcauchy(x)/.5)
#SE mu.is
semu.is <- (1/samp.size)*var(dexp(x)* wts)
print(semu.is)

#SE mu.is standard
semu.is <- (1/samp.size)*var(dexp(x)* wts) + mu.is^2*var(wts) - 2*mu.is*cov(dexp(x)*wts,wts)
print(semu.is)

# Importance sampling estimate of the mean
mu.is = (1/samp.size)*sum(sqrt(x)*wts)
mu.is

#variance of the weights and effective sample size
var(wts)
samp.size/(1+var(wts)) #ESS

#Importance sampling with standardized weights, estimated mean
mu.sis = sum(sqrt(x)*wts)/sum(wts)
mu.sis
