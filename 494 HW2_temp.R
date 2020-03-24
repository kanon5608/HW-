library("ElemStatLearn")
data(prostate)
str( prostate )
abserror<-matrix(NA,100,7)
# (I) full linear model; (II) reduced linear model (4) with lcavol, lweight, lbph, svi; 
#(III) reduced linear model (2) with lcavol, lweight; 
#(IV) subset selection using R function step; (V) Ridge regression; (VI) Lasso; (VII) Lars.
colnames(abserror)=c("I","II","III","Iv","V","VI","VII")
squerror=abserror

set.seed(5)
#loop
ix <- 0
while (ix <= 99){
  ix <-ix+1
  ind=sample(1:97,size=67,replace=F);
  train=prostate[ind,1:9];
  test=prostate[-ind,1:9];
  #data settings
  trainst <- train
  for(i in 1:8) {
    trainst[,i] <- trainst[,i] - mean(prostate[,i]);
    trainst[,i] <- trainst[,i]/sd(prostate[,i]);
  }
  testst <- test
  for(i in 1:8) {
    testst[,i] <- testst[,i] - mean(prostate[,i]);
    testst[,i] <- testst[,i]/sd(prostate[,i]);
  }
  #regerssion model[(I)(ii)(III)]
  fitls <- lm( lpsa ~ lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, data=trainst ) #I
  fitlsr4 <- lm( lpsa ~ lcavol+lweight+lbph+svi, data=trainst )                     #II
  fitlsr2 <- lm( lpsa ~ lcavol+lweight, data=trainst )                              #III
  #(I) full linear model
  test.fitls=predict(fitls, newdata=testst) 
  abserror[ix,1]=mean(abs(test[,9]-test.fitls))
  squerror[ix,1]=mean((test[,9]-test.fitls)^2)
  #(II) reduced linear model
  test.fitlsr4=predict(fitlsr4, newdata=testst) 
  abserror[ix,2]=mean(abs(test[,9]-test.fitlsr4))
  squerror[ix,2]=mean((test[,9]-test.fitlsr4)^2)
  #(III) reduced linear model (2) with lcavol, lweight
  test.fitlsr2=predict(fitlsr2, newdata=testst) 
  abserror[ix,3]=mean(abs(test[,9]-test.fitlsr2))
  squerror[ix,3]=mean((test[,9]-test.fitlsr2)^2)
}
#(IV) subset selection using R function step
fitls <- lm( lpsa ~ lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, data=trainst )
step(fitls)
fitlsAIC <- lm(formula = lpsa ~ lcavol + lweight + svi, data = trainst)
ix <- 0 
while(ix <= 99){
  ix <- ix + 1
  test.fitlsrAIC=predict(fitlsAIC, newdata=testst) 
  abserror[ix,4]=mean(abs(test[,9]-test.fitlsAIC))
  squerror[ix,4]=mean((test[,9]-test.fitlsr2)^2)
}
#(V) Ridge regression
prostate.ridge <- simple.ridge( trainst[,1:8], trainst[,9], df=seq(1,8,by=0.5) )
library("glmnet")
cv.out=cv.glmnet(x=as.matrix(trainst[,1:8]), y=as.numeric(trainst[,9]), nfolds=10, alpha=0, standardize=F)
#show result
#abserror
#squerror
#merge(abserror,squerror)
#partial t-test
#round(cbind(abserror,squerror),3)
