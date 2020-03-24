#install.pakages("ElemStatLearn")
library("ElemStatLearn")
#install.packages("lasso2")
library(lasso2)
#install.packages("lars")
library(lars)
data(prostate)
str( prostate )
abserror<-matrix(NA,100,7)
# (I) full linear model; (II) reduced linear model (4) with lcavol, lweight, lbph, svi; 
#(II}I) reduced linear model (2) with lcavol, lweight; 
#(IV) subset selection using R function step; (V) Ridge regression; (VI) Lasso; (VII) Lars.
colnames(abserror)=c("I","II","III","Iv","V","VI","VII")
squerror=abserror

#data settings
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
  #(IV) subset selection using R function step
  fitls <- lm( lpsa ~ lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, data=trainst )
  step(fitls,trace = 0);
  fitlsAIC <- lm(formula = lpsa ~ lcavol + lweight + svi, data = trainst)
  test.fitAIC=predict(fitlsAIC, newdata=testst) 
  abserror[ix,4]=mean(abs(test[,9]-test.fitAIC))
  squerror[ix,4]=mean((test[,9]-test.fitAIC)^2)
  
  #(V) Ridge regression
  prostate.ridge <- simple.ridge( trainst[,1:8], trainst[,9], df=seq(1,8,by=0.5) )

  cv.out=cv.glmnet(x=as.matrix(trainst[,1:8]), y=as.numeric(trainst[,9]), nfolds=10, alpha=0, standardize=F)
  lambda.10fold=cv.out$lambda.1s
  fitridge=glmnet(x=as.matrix(trainst[,1:8]),y=as.numeric(trainst[,9]),alpha=0,lambda=lambda.10fold,standardize=F,thresh=1e-12)
  test.ridge=predict(fitridge,newx=as.matrix(testst[,1:8]))
  abserror[ix,5]=mean(abs(test[,9]-test.ridge))
  squerror[ix,5]=mean((test[,9]-test.ridge)^2)
  
  #(VII) Lars

  prostate.lar <- lars(x=as.matrix(trainst[,1:8]), y=as.numeric(trainst[,9]), type="lar", trace=FALSE, normalize=F);
  cv.out <- cv.lars(x=as.matrix(trainst[,1:8]), y=as.numeric(trainst[,9]), K=10, plot.it=F, type="lar", trace=FALSE, normalize=F);
  itemp=which.min(cv.out$cv);
  k.lars = min(cv.out$index[cv.out$cv < cv.out$cv[itemp]+cv.out$cv.error[itemp]]);
  #test error
  test.lars=predict(prostate.lar, newx=as.matrix(testst[,1:8]), s=k.lars, type="fit", mode=cv.out$mode)$fit;
  abserror[ix,7]=mean(abs(test[,9]-test.lars))
  squerror[ix,7]=mean((test[,9]-test.lars)^2)
}

#(VI)Lasso
#install.packages("lasso2")
library(lasso2)
ix <- 0
while(ix<=99){
  ix <- ix+1
  prostate.lasso <- l1ce( lpsa ~ ., data=trainst, trace=FALSE, sweep.out=~1, bound=seq(0,1,by=0.1) )
  prostate.lasso.coef <- sapply(prostate.lasso, function(x) x$coef)
  colnames(prostate.lasso.coef) <- seq( 0,1,by=0.1 );
  cv.out=cv.glmnet(x=as.matrix(train[,1:8]), y=as.numeric(train[,9]), nfolds=10, alpha=1)
  lambda.10fold=cv.out$lambda.1s
  #model
  fitlasso=glmnet(x=as.matrix(trainst[,1:8]),y=as.numeric(trainst[,9]),alpha=1,lambda=lambda.10fold,standardize=F,thresh=1e-12)
  #estimate errors
  test.lasso=predict(fitlasso,newx=as.matrix(testst[,1:8]))
  abserror[ix,6]=mean(abs(test[,9]-test.lasso))
  squerror[ix,6]=mean((test[,9]-test.lasso)^2)
}

#show result
round(cbind(abserror,squerror),3)
#partial t-test
for(i in 1:6) for (j in (i+1):7){
  t.test(abserror[,i],abserror[,j])
}

