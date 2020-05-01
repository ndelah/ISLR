# Chapter 3: Linear Regression ----
# 3.6 Lab: Linear Regression ====
### 3.6.1 Libraries ####
library(MASS)
library(ISLR)

### 3.6.2 Simple Linear Regression ####
fix(Boston)
names(Boston)
?Boston

lm.fit=lm(medv~lstat,Boston)

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

### 3.6.3 Multiple Linear Regression ####
# On two variables
lm.fit= lm(medv~lstat+age,data=Boston)
summary(lm.fit) # All predictors are significant, F statistic is significant and R squared low

# On all variables
lm.fit = lm(medv~.,data=Boston)
summary(lm.fit)
# Rsquared is definitely bette.  age and indus not significant


### 3.6.4 Interaction Terms #### 
summary(lm(medv~lstat*age,data=Boston))


### 3.6.5 Non-linear Transformations of the Predictors #### 
# I() is a necessary wrapping to create non linear transformations as ^ has diffrent meaning
lm.fit2 = lm(medv~lstat +I(lstat^2),data=Boston)
summary(lm.fit2)
plot(lm.fit2) # The adjusted R squared is 0.63 which is much better for just one variable
anova(lm.fit,lm.fit2) #The p test confirms that the two models are significantly different from each other
par(mfrow=c(2,2))
plot(lm.fit2)
# Higher order polynomials
lm.fit5=lm(medv~poly(lstat,5),data=Boston)
summary(lm.fit5)
