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

# Basic plot
# The data is not linear
plot(Boston$lstat,Boston$medv)
abline(lm.fit, lwd=3)
abline(lm.fit, lw=3, col="red")
plot(Boston$lstat,Boston$medv,col="red")
plot(Boston$lstat,Boston$medv,pch=20)
plot(Boston$lstat,Boston$medv,pch="+")
plot(1:20,1:20,pch=1:20)

#Splitting into multiple plots
par(mfrow=c(2,2))
plot(lm.fit)


plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit))
plot(residuals(lm.fit))
plot(lm.fit)

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

