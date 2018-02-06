library(ISLR)
library(ggplot2)
library(gridExtra)
library(MASS)
library(class)
library(caTools)

data(Weekly)
head(Weekly)
attach(Weekly)

summary(Weekly)
pairs(Weekly, col=Direction)

a = ggplot(Weekly, aes(x=Year, Lag1, color=Direction)) + geom_point() + geom_smooth()
b = ggplot(Weekly, aes(x=Year, Lag2, color=Direction)) + geom_point() + geom_smooth()
c = ggplot(Weekly, aes(x=Year, Lag3, color=Direction)) + geom_point() + geom_smooth()
d = ggplot(Weekly, aes(x=Year, Lag4, color=Direction)) + geom_point() + geom_smooth()
e = ggplot(Weekly, aes(x=Year, Lag5, color=Direction)) + geom_point() + geom_smooth()
grid.arrange(a,b,c,d,e, nrow=2, ncol=3)


# There are no patterns present, which is to be expected from stock data, otherwise I wouldn't be sitting here repeating 
# the same joke used twice within 10 minutes of each other in the R Session lecture videos, otherwise I'd be rich!

glm1 = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Weekly, family="binomial")
summary(glm1)
# Lag2 seems to be statistically significant with a p-value of 0.0296

glm1.prob = predict(glm1, Weekly, type="response")
glm1.pred = rep("Down", nrow(Weekly))
glm1.pred[glm1.prob > 0.5] = "Up"
table(glm1.pred, Direction)
(54+557)/(54+557+430+48)
54/(54+430)
# It seems like the model is classifying a lot of the down days as up days, therefore making a lot of false positive errors. 
# It is however correctly classifying 56% of the time (really good score), however it's only getting 11% of the down days correct. 

detach(Weekly)
train = subset(Weekly, Weekly$Year >= 1990 & Weekly$Year < 2009)
test = subset(Weekly, Weekly$Year > 2008)

glm2 = glm(Direction ~ Lag2, train, family="binomial")
summary(glm2)
glm2.prob = predict(glm2, test, type="response")
glm2.pred = rep("Down", nrow(test))
glm2.pred[glm2.prob > 0.5] = "Up"
table(glm2.pred, test$Direction)
(9+56)/(9+56+34+5)
# Seemed to have got a very good score of 62.5% correct classifications. 

lda1 = lda(Direction ~ Lag2, train)
summary(lda1)
plot(lda1)
lda1.prob = predict(lda1, test, type="response")
lda1.pred = lda1.prob$class
# The predict function for lda conviniently gives us the class predictions, so no need to build a predict vector
table(lda1.pred, test$Direction)

# This has given us the exact same result as the logistic regression model

qda1 = qda(Direction ~ Lag2, train)
summary(qda1)
qda1.prob = predict(qda1, test, type="response")
qda1.pred = qda1.prob$class
# The predict function for QDA conviniently gives us the class predictions, so no need to build a predict vector.
table(qda1.pred, test$Direction)
61/(61+43)
# 53% is a downgrade on the 62.4% we got with logistic regression and LDA. 

train.X = as.matrix(train$Lag2)
test.X = as.matrix(test$Lag2)
train.Direction = train$Direction
k = 1
set.seed(99)
knn.pred = knn(train.X, test.X, train.Direction,k)
table(knn.pred, test$Direction)
(21+31)/(21+30+22+31)
# Poor result with 50% correct classifications, no better than chance. This is with K = 1. 

data(Auto)
attach(Auto)

Auto = mutate(Auto, mpg01 = ifelse(mpg < median(mpg), 0, 1))
head(Auto)
table(Auto$mpg01)
# Changing colour of pairs plot    http://stackoverflow.com/questions/15599717/coloring-points-in-a-pairs-plot
cols <- character(nrow(Auto))
cols[] <- "black"

cols[Auto$mpg01 == 1] <- "black"
cols[Auto$mpg01 == 0] <- "red"
pairs(Auto, col=cols)
# Horsepower, weight displaement and acceleration seem like good variables to use as predictors. 
cor(Auto$horsepower, Auto$acceleration)
# Horsepower and acceleration seem to have a high correlation between themselve, therefore may be a good idea to only use one of them.
plot(acceleration ,Auto$mpg01+rnorm(392,mean=0,sd=0.1), pch=20)
plot(horsepower ,Auto$mpg01+rnorm(392,mean=0,sd=0.1), pch=20)
plot(weight ,Auto$mpg01+rnorm(392,mean=0,sd=0.1), pch=20)
plot(displacement ,Auto$mpg01+rnorm(392,mean=0,sd=0.1), pch=20)
# The plots above were created with some noise around the the two points. This helps us get a sense of how many data points
# lie on either 1 or 0, something which is hard to tell from how it's originally presented. 

split = sample.split(Auto$mpg01, 0.6)
test = subset(Auto, split == TRUE)
train = subset(Auto, split == FALSE)

lda1 = lda(mpg01 ~ weight + horsepower + acceleration, train)
summary(lda1)
plot(lda1)
lda1.prob = predict(lda1, test)
names(lda1.prob)
head(lda1.prob$class)
table(lda1.prob$class, test$mpg01)
1 - (93+110)/(93+110+8+25)
# The LDA model gives a 14% test error rate

qda1 = qda(mpg01 ~ weight + horsepower + acceleration, train)
summary(qda1)
qda1.prob = predict(qda1, test)
names(qda1.prob)
head(qda1.prob$class)
table(qda1.prob$class, test$mpg01)
1 - (96+109)/(96+109+9+22)
# The QDA model gives a 13.2% test error rate

glm1 = glm(mpg01 ~ weight + horsepower + acceleration, train, family="binomial")
summary(glm1)
glm1.prob = predict(glm1, test, type="response")
names(glm1.prob)
head(glm1.prob)
glm1.pred = rep(0, length(glm1.prob))
glm1.pred[glm1.prob > 0.5] = 1
table(glm1.pred, test$mpg01)
1 - (93+104)/(98+104+14+20)
# The Logistic Regression model gives a 16.5% test error rate

train.X = as.matrix(cbind(train$weight, train$horsepower, train$acceleration))
test.X = as.matrix(cbind(test$weight, test$horsepower, test$acceleration))
train.mpg01 = train$mpg01
k = 100
set.seed(99)
knn.pred = knn(train.X, test.X, train.mpg01,k)
table(knn.pred, test$mpg01)
k1 = 1 - (99+105)/(99+105+13+19)
k5 = 1 - (101+102)/(101+102+17+16)
k3 = 1 - (100+102)/(100+102+16+18)
k2 = 1 - (93+101)/(99+101+17+25)
# k1 seems to have the best error rate at 13.6% 


Power = function(){
  2^3
}

Power2 = function(x,a){
  x^a
}


Power3 = function(x,a){
  result = x^a
  return(result)
}

plot(Power3(1:10, 2), log="xy", ylab="Log of y = x^2", xlab="Log of x",
     main="Log of x^2 versus Log of x")

PlotPower = function(x,a){
  plot(x, x^a)
}


data(Boston)
names(Boston)

Boston = mutate(Boston, crimeRate = ifelse(crim > median(crim), 1, 0))
head(Boston)
attach(Boston)
table(crimeRate)

cols <- character(nrow(Boston))
cols[] <- "black"

cols[Boston$crimeRate == 1] <- "black"
cols[Boston$crimeRate == 0] <- "red"

pairs(Boston, col=cols)
# nos, lstat and dis seem like they could be good predictors to use, we will revisit this question in chapter 6!

split = sample.split(Boston$crimeRate, 0.6)
train = subset(Boston, split == TRUE)
test = subset(Boston, split == FALSE)

glm1 = glm(crimeRate ~.-crim-crimeRate, train, family="binomial")
summary(glm1)
glm1.prob = predict(glm1, test, type="response")
names(glm1.prob)
head(glm1.prob)
glm1.pred = rep(0, length(glm1.prob))
glm1.pred[glm1.prob > 0.5] = 1
table(glm1.pred, test$crimeRate)
1 - (90+91)/(90+91+11+10)
 # 10.4% error rate

lda1 = lda(crimeRate~.-crimeRate-crim, train)
summary(lda1)
plot(lda1)
lda1.prob = predict(lda1, test)
names(lda1.prob)
head(lda1.prob$class)
table(lda1.prob$class, test$crimeRate)
1 - (94+75)/(94+75+7+26)
# 16.3% error rate 

qda1 = qda(crimeRate~.-crimeRate-crim, train)
summary(qda1)
qda1.prob = predict(qda1, test)
names(qda1.prob)
head(qda1.prob$class)
table(qda1.prob$class, test$crimeRate)
1 - (100+83)/(100+83+18+1)
# 9.4% error rate 

train.X = as.matrix(train[,c(-1,-15)])
test.X = as.matrix(test[,c(-1,-15)])
train.crimeRate = train$crimeRate
k = 10
set.seed(99)
knn.pred = knn(train.X, test.X, train.crimeRate,k)
table(knn.pred, test$crimeRate)
k1 =  1 - (90+94)/(90+94+7+11)
# 8.9% error rate 
k10 = 1 - (95+82)/(95+82+19+6)
# 12.4% error rate
