library(ISLR2)
set.seed(1)

#Validation Set 
train = sample(392,196) #196 elements from 1:392
lm.fit = lm(mpg~horsepower, data=Auto, subset=train) #using only train 
attach(Auto)
mean((mpg - predict(lm.fit,Auto))[-train]^2) #all those not in train
lm.fit2 = lm(mpg~poly(horsepower,2), data=Auto, subset=train) #quadratic lm 
mean((mpg - predict(lm.fit2,Auto))[-train]^2) #all those not in train
lm.fit3 = lm(mpg~poly(horsepower,3), data=Auto, subset=train) #cubic lm 
mean((mpg - predict(lm.fit3,Auto))[-train]^2) #all those not in train

set.seed(2) #different train set & validation set
train = sample(392,196) #196 elements from 1:392
lm.fit = lm(mpg~horsepower, data=Auto, subset=train) #using only train 
mean((mpg - predict(lm.fit,Auto))[-train]^2) #all those not in train
lm.fit2 = lm(mpg~poly(horsepower,2), data=Auto, subset=train) #quadratic lm 
mean((mpg - predict(lm.fit2,Auto))[-train]^2) #all those not in train
lm.fit3 = lm(mpg~poly(horsepower,3), data=Auto, subset=train) #cubic lm 
mean((mpg - predict(lm.fit3,Auto))[-train]^2) #all those not in train

#LOOCV
glm.fit=glm(mpg~horsepower, data=Auto) #without family, same as lm 
coef(glm.fit)

library(boot)
glm.fit = glm(mpg~horsepower, data=Auto)
cv.err = cv.glm(Auto, glm.fit)
cv.err$delta #cross validation results
cv.error = rep(0,10)
for (i in 1:10){ #different polynomial models and their CV results
  glm.fit = glm(mpg~poly(horsepower,i), data=Auto)
  cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
} 
cv.error

#k-fold CV
set.seed(17)
cv.error.10 = rep(0,10)
for(i in 1:10){ #poly models for 10-fold CV
  glm.fit = glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10 

#Bootstrap: Accuracy of Statistic
#custom function to find alpha
alpha.fn = function(data,index) { 
  X = data$X[index]
  Y = data$Y[index]
  (var(Y) - cov(X,Y))/(var(X) + var(Y) - 2*cov(X,Y))
}

alpha.fn(Portfolio, 1:100) #using all 100 observations

set.seed(7) 
alpha.fn(Portfolio, sample(100,100,replace=T)) #constructing bootstrap dataset,100 obs with replacement
boot(Portfolio, alpha.fn, R=1000) #from 1000 estimates

#Bootstrap: Accuracy of LR model
boot.fn = function(data,index)
  coef(lm(mpg~horsepower,data=data, subset = index))
boot.fn(Auto, 1:392)

set.seed(1)
boot.fn(Auto, sample(392,392,replace=T)) #sample with replacement
boot.fn(Auto, sample(392,392,replace=T))

boot(Auto, boot.fn, 1000)
summary(lm(mpg~horsepower,data=Auto))$coef #estimates not so close

#fitting quadratic model 
boot.fn = function(data,index)
  coef(lm(mpg~horsepower + I(horsepower^2),data=data, subset = index))
set.seed(1)
boot(Auto, boot.fn,1000)
summary(lm(mpg~horsepower + I(horsepower^2),data=Auto))$coef #estimates closer, better model fit
