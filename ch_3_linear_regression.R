# Chapter 3: Linear Regression ----
# 3.6 Lab: Linear Regression ====
### Libraries ####
library(MASS)
library(ISLR)

### 3.6.2 Simple Linear Regression
fix(Boston)
names(Boston)
?Boston

lm.fit=lm(medv~lstat,Boston)
lm.fit

summary(lm.fit) # lstat is statistically significant from 0, but the model doesn't explain much
names(lm.fit)
lm.fit$coefficients

# Confidence interval for coefficients:
confint(lm.fit)
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval="confidence")
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval="prediction")

# lkjdflk