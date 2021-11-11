library(ISLR2)
##Stock Market Data
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket)
cor(Smarket[,-9]) #direction not qualitative
attach(Smarket)
plot(Volume)

##Logistic Regression
glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fits)
coef(glm.fits)
glm.probs = predict(glm.fits, type='response') #type=response means prob and not logit values are outputted
glm.probs[1:10]
contrasts(Direction) #dummy variable for Direction made by R
glm.pred = rep("Down",1250)
glm.pred[glm.probs>0.5] = "Up"
table(glm.pred, Direction) #Confusion Matrix
mean(glm.pred == Direction) #Correctly classified => 52%

train = (Year < 2005) #Boolean Vector for classifying train and test data
Smarket.2005 = Smarket[!train,]  #Creating train (before 2005) and test (2005) data
dim(Smarket.2005)
Direction.2005 = Direction[!train]

glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train) #subset means trained only on 2005
glm.probs = predict(glm.fits, Smarket.2005, type="response")
glm.pred = rep("Down", 252)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005) #prediction = 0.48, worse than random guessing

glm.fits = glm(Direction~Lag1 + Lag2, data = Smarket, family = binomial, subset = train) #only on Lag1 and Lag2, other p-values small
glm.probs = predict(glm.fits, Smarket.2005, type="response")
glm.pred = rep("Down", 252)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)

predict(glm.fits, newdata=data.frame(Lag1=c(1.2,1.5), Lag2 = c(1.1,-0.8)), type="response")

#LDA 
library(MASS)
lda.fit = lda(Direction~Lag1 + Lag2, data = Smarket, subset=train)
lda.fit
plot(lda.fit)
lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class = lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)

sum(lda.pred$posterior[,1] >=0.5) #0.5 cutoff for groups -> 70 predicted down
sum(lda.pred$posterior[,1] <=0.5) #182 predicted up 
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>0.9) #changing posterior prob. cutoff to 0.9

#QDA
qda.fit = qda(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
qda.fit

qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)
