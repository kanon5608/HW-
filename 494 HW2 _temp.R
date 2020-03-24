#HW3 494 Sicheng Tian
# load R packages
library("ElemStatLearn")  # for prostate cancer dataset
#install.packages("pls")
library(pls)

# load the data set "prostate"
data(prostate)

set.seed(432)                       # specify the intital random seed
train.index <- matrix(0, 100, 97) # each row indicates one set of simulated training indices
for(i in 1:100) train.index[i,]=sample(x=c(rep(1,67),rep(0,30)), size=97, replace=F)   # generate random indices of training data

# define matrices to store results
APerror <- matrix(0, 100, 4)      # mean (absolute) prediction error
SPerror <- matrix(0, 100, 4)      # mean (squared) prediction error
colnames(APerror)=colnames(SPerror)=c("FullModel", "ReducedModel(4)", "PCR", "PLS")
Tuning <- APerror                   # record values of tuning parameters

ttemp=proc.time()                   # record computing time
for(ix in 1:100) {             # start of loop with "isimu"
  # partition the original data into training and testing datasets
  train <- subset( prostate, train.index[ix,]==1 )[,1:9]
  test  <- subset( prostate, train.index[ix,]==0 )[,1:9]  
  # fit linear model on training dataset using LS method
  trainst <- train
  for(i in 1:8) {
    trainst[,i] <- trainst[,i] - mean(prostate[,i]);
    trainst[,i] <- trainst[,i]/sd(prostate[,i]);
  }
  fitls <- lm( lpsa ~ lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, data=trainst )
  fitlsr4 <- lm( lpsa ~ lcavol+lweight+lbph+svi, data=trainst )
  ## check testing errors
  testst <- test
  for(i in 1:8) {
    testst[,i] <- testst[,i] - mean(prostate[,i]);
    testst[,i] <- testst[,i]/sd(prostate[,i]);
  }
  
  ## (I) mean prediction error based on full model
  test.fitls=predict(fitls, newdata=testst)  
  # mean (absolute) prediction error
  APerror[ix,1]=mean(abs(test[,9]-test.fitls))  
  # mean (squared) prediction error
  SPerror[ix,1]=mean((test[,9]-test.fitls)^2)  
  
  ## (II) mean prediction error based on reduced model(4)
  test.fitlsr=predict(fitlsr4, newdata=testst)  
  # mean (absolute) prediction error
  APerror[ix,2]=mean(abs(test[,9]-test.fitlsr))  
  # mean (squared) prediction error
  SPerror[ix,2]=mean((test[,9]-test.fitlsr)^2)                 
  
  # (III)Principal Components Regression
  pcr.fit=pcr(lpsa~., data=trainst, scale=F, validation="CV", segments=10)
  
  # find the best number of components, regenerating part of Figure 3.7 on page 62
  itemp=which.min(pcr.fit$validation$PRESS)     
  itemp.mean=pcr.fit$validation$PRESS[itemp]/67 
  mean((pcr.fit$validation$pred[,,itemp]-trainst[,9])^2) 
  itemp.sd=sd((pcr.fit$validation$pred[,,itemp]-trainst[,9])^2)/sqrt(67)   
  k.pcr = min((1:pcr.fit$validation$ncomp)[pcr.fit$validation$PRESS/67 < itemp.mean+itemp.sd])  

  # estimating mean prediction error
  test.pcr=predict(pcr.fit,as.matrix(testst[,1:8]),ncomp=k.pcr)
  # mean (absolute) prediction error
  APerror[ix,3]=mean(abs(test[,9]-test.pcr))               
  # mean (squared) prediction error
  SPerror[ix,3]=mean((test[,9]-test.pcr)^2)                
  
  # (IV)PLS
  plsr.fit=plsr(lpsa~., data=trainst, scale=F, validation="CV", segments=10)
  
  # find the best number of components, regenerating part of Figure 3.7 on page 62
  itemp=which.min(plsr.fit$validation$PRESS)     
  itemp.mean=plsr.fit$validation$PRESS[itemp]/67 
  itemp.sd=sd((plsr.fit$validation$pred[,,itemp]-trainst[,9])^2)/sqrt(67)  
  k.plsr = min((1:plsr.fit$validation$ncomp)[plsr.fit$validation$PRESS/67 < itemp.mean+itemp.sd])  
  
  # estimating mean prediction error
  test.plsr=predict(plsr.fit,as.matrix(testst[,1:8]),ncomp=k.plsr)
  # mean (absolute) prediction error
  APerror[ix,4]=mean(abs(test[,9]-test.plsr))               
  # mean (squared) prediction error
  SPerror[ix,4]=mean((test[,9]-test.plsr)^2)                

}                                                               # end of loop with "ix"
proc.time()-ttemp
# user  system elapsed 
# 21.04    0.35   23.22  

## plot the output
par(mfrow=c(1,1))
# mean (absolute) prediction error
boxplot(APerror)
# mean (squared) prediction error
boxplot(SPerror)

## check summary statistics
# mean (absolute) prediction error
summary(APerror)

# mean (squared) prediction error
summary(SPerror)



# test the difference on mean (absolute) prediction error
pvalue.APE <- matrix(1, 4, 4)# matrix with (i,j)-entry indicating p-values of corresponding paired t-tests on mean (absolute) predition error
rownames(pvalue.APE)=colnames(pvalue.APE)=c("FullModel", "ReducedModel(4)", "PCR","PLS")
pvalue.SPE <- pvalue.APE     # matrix with (i,j)-entry indicating p-values of corresponding paired t-tests on mean (squared) prediction error
for(i in 1:3) for(j in (i+1):4) {
  pvalue.APE[i,j]=pvalue.APE[j,i]=t.test(x=APerror[,i], y=APerror[,j], alternative="two.sided", paired=T)$p.value;
  pvalue.SPE[i,j]=pvalue.SPE[j,i]=t.test(x=SPerror[,i], y=SPerror[,j], alternative="two.sided", paired=T)$p.value;
}



## median of APerror
round(pvalue.APE,3)
round(apply(APerror,2,median),4)
sort(apply(APerror,2,median))

round(pvalue.SPE,3)
round(apply(SPerror,2,median),4)
sort(apply(SPerror,2,median))


## how many times one method is better than another in terms of mean (absolute) prediction error
count.better.AP <- matrix(0, 4, 4)  # entry (i,j): how many times method i is better than method j 
rownames(count.better.AP)=colnames(count.better.AP)=c("FullModel", "ReducedModel(4)", "PCR", "PLS")
count.better.SP <- count.better.AP
for(i in 1:4) for(j in 1:4) {
  count.better.AP[i,j] = sum(APerror[,i] < APerror[,j]);
  count.better.SP[i,j] = sum(SPerror[,i] < SPerror[,j]);
}
count.better.AP

count.better.SP


## check tuning parameters
summary(Tuning[,3:4])