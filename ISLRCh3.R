library(MASS)
library(ISLR2)

#LR
head(Boston)
attach(Boston)
lm.fit = lm(medv ~ lstat)
lm.fit
summary(lm.fit)
confint(lm.fit)
predict(lm.fit, data.frame(lstat = (c(5,10,15))), interval = 'confidence')
predict(lm.fit, data.frame(lstat = (c(5,10,15))), interval = 'prediction')
plot(lstat,medv)
abline(lm.fit)
abline(lm.fit, lwd = 3)
abline(lm.fit, lwd = 3, col="red")
par(mfrow = c(2,2))
plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))

#MLR
lm.fit = lm(medv~lstat+age)
summary(lm.fit)
lm.fit = lm(medv ~ ., data = Boston)
summary(lm.fit)
library(car)
vif(lm.fit)
lm.fit1 = lm(medv~.-age, data=Boston)
summary(lm.fit1)
lm.fit1 = update(lm.fit, ~. - age)

#Interaction Terms
summary(lm(medv~lstat*age))

#Non-Linear Transform of Predictors
lm.fit2 = lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
lm.fit = lm(medv~lstat)
anova(lm.fit, lm.fit2)

lm.fit5 = lm(medv~poly(lstat,5))
summary(lm.fit5)

#Qualitative Predictors
head(Carseats)
attach(Carseats)
lm.fit = lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)
contrasts(ShelveLoc)
