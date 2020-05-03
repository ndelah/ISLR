# Chapter 5: Resampling Methods ----
# 5.3 Lab: Cross-Validation and the Bootstrap ====
## 5.3.1 The validation set approach ####

library(ISLR)
set.seed(1)
train=sample(392,196)
lm.fit=lm(mpg ~ horsepower,data=Auto,subset=train)
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)


lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

# Previous findings are supported. The quadratic model works better than the cubic and linear model


# 5.3.4. The Bootstrap ####
alpha.fn = function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

set.seed(1)
alpha.fn(Portfolio,1:100)

alpha.fn(Portfolio,sample(100,100,replace=T))

install.packages(boot)
load(boot)
boot(Portfolio,alpha.fn,R=1000)


boot.fn=function(data,index)
  return(coef(lm(mpg~horsepower,data=data,subset=index)))
boot.fn(Auto,1:392)
boot(Auto,boot.fn,1000)
