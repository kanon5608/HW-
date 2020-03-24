#install.packages("dvmisc")
#install.packages("DescTools")
library(dvmisc)
data <- read.csv("535P3.csv")
yield <- data[,1]
acid <- data[,2]
batch <- data[,3]
time <- data[,4]
time <- factor(time, levels = c('alpha', 'beta', 'gamma', 'delta', 'epsilon'))
mydata <- data.frame(yield, acid, batch, time)

par(mfrow=c(2,2))
plot(yield ~ acid+batch+time,mydata)
#a)
fit <- lm(yield ~ acid+batch+time,mydata)  
anova(fit)  
#Analysis of Variance Table

#Response: yield
#Df Sum Sq Mean Sq F value Pr(>F)
#acid       1  12.50  12.500  0.4944 0.4927
#batch      4  13.04   3.260  0.1289 0.9695
#time       4  10.24   2.560  0.1012 0.9803
#Residuals 15 379.26  25.284  

#b)
fit.2 <-lm(yield ~batch+time,mydata)
anova(fit.2)
#Analysis of Variance Table

#Response: yield
#Df Sum Sq Mean Sq F value Pr(>F)
#batch      4  13.04   3.260  0.1331 0.9679
#time       4  10.24   2.560  0.1046 0.9793
#Residuals 16 391.76  24.485 
get_mse(fit)/get_mse(fit.2)

#c)
library(DescTools)
fit.aov <- aov(yield ~ acid+batch+time,mydata)  
PostHocTest(fit.aov, 
            method = "bonferroni", 
            conf.level = 0.9)
t.test(acid, conf.level = 0.9, p.adjust.methods = "bonferroni")
BonfCI<-pairwiseCI(yield ~ acid, data=mydata,
                       method="Param.ratio", alternative="less",
                       var.equal=FALSE, conf.level=0.9)