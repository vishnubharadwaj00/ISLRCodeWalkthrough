matrix(data = c(1,2,3,4), nrow=2, ncol = 2)
matrix(data = c(1,2,3,4), nrow=2, ncol = 2, byrow=TRUE)

library(ISLR2)
head(Auto)

attach(Auto)
plot(horsepower, mpg)
identify(horsepower, mpg, name)
