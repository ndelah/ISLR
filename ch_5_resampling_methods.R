# Chapter 5: Resampling Methods ----
# 5.3 Lab: Cross-Validation and the Bootstrap ====
## 5.3.1 The validation set approach

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
