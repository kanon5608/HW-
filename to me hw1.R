library("ElemStatLearn")
data(prostate)
str( prostate )
abserror<-matrix(NA,100,3)
colnames(abserror)=c("Train Model","Full Model","Reduced Model")
squerror=abserror

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
  #regerssion model
  fitls <- lm( lpsa ~ lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, data=trainst ) #full
  fitlsr <- lm( lpsa ~ lcavol+lweight+lbph+svi, data=trainst ) #reduced
  #train
  mean(trainst[,9])
  abserror[ix,1]=mean(abs(testst[,9]-mean(trainst[,9])))
  squerror[ix,1]=mean((testst[,9]-mean(trainst[,9]))^2)
  #full
  test.fitls=predict(fitls, newdata=testst) 
  abserror[ix,2]=mean(abs(test[,9]-test.fitls))
  squerror[ix,2]=mean((test[,9]-test.fitls)^2)
  #reduced
  test.fitlsr=predict(fitlsr, newdata=testst) 
  abserror[ix,3]=mean(abs(test[,9]-test.fitlsr))
  squerror[ix,3]=mean((test[,9]-test.fitlsr)^2)
}
#show result
abserror
squerror
merge(abserror,squerror)
#partial t-test
round(cbind(abserror,squerror),3)
t.test(abserror[,1],abserror[,2],paired=T,alternative="greater")
t.test(abserror[,2],abserror[,3],paired=T,alternative='greater')
t.test(abserror[,2],abserror[,3],paired=T,alternative='two.side')
t.test(squerror[,1],squerror[,3],paired=T,alternative='greater')
t.test(squerror[,2],squerror[,3],paired=T,alternative='greater')