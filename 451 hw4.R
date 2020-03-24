#451 Hw4
#Sicheng Tian
#a)
#install.packages("class")
library(class)
set.seed(101)
y=c(rep(1,25),rep(2,25))
cvind = rep(1:5,10)
error_a <- rep(0,50)
for(isimu in 1:50){
  x<-rnorm(5000*50)
  dim(x)=c(50,5000)
  xcor1=cor(y,x)
  summary(xcor1)
  ind1=order(-abs(xcor1))[1:100]
  x1=x[,ind1]
  rind=sample(cvind,size=50,replace=F)
  for(icv in 1:5){
    ypred=knn(x1[rind!=icv,],x1[rind==icv,],cl=y[rind!=icv],k=1)
    error_a[isimu]=error_a[isimu]+sum(ypred!=y[rind==icv]);
  }
}
print(error_a/50/length(y)) #show the error rate
mean(error_a/50/length(y))

#b)
y=c(rep(1,25),rep(2,25))
dim(y)<-c(50,1)
cvind = rep(1:5,10)
error_b <- rep(0,50)
for(isimu in 1:50){
  x <- rnorm(50*5000)
  dim(x) <- c(50,5000)
  for(icv in 1:5){
    rind=sample(cvind,size=50,replace=F) #random divide into 5 folds as icv from 1 to 5
    test_f <- which(rind==icv,arr.ind=TRUE) #testindex
    train <- x[-test_f, ] #training data
    test <- x[test_f, ] #testing data
    xcor2_train <- cor(y[rind!=icv,],train)
    xcor2_test <- cor(y[rind==icv,],test)
    ind2_train=order(-abs(xcor2_train))[1:100] #find the 100 biggest correlations in training data
    ind2_test=order(-abs(xcor2_test))[1:100] #~in testing data
    x2_train=x[,ind2_train] #select training data 
    x2_test=x[,ind2_test] #select testing data
    ypred=knn(x2_train[rind!=icv,],x2_test[rind==icv,],cl=y[rind!=icv],k=1) #1-nearest cv
    error_b[isimu]=error_b[isimu]+sum(ypred!=y[rind==icv]);
  }
}
print(error_b/length(y)) #show the error rate 
mean(error_b/length(y))