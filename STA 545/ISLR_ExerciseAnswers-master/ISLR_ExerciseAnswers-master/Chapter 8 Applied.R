library(randomForest)
library(caTools)
library(MASS)
library(ggplot2)
library(tree)
library(ISLR)
library(rpart)
library(rpart.plot)
library(rattle)
library(e1071)
library(caret)
library(gbm)

data(Boston)
split = sample.split(Boston$medv, 0.6)
train = subset(Boston, split==TRUE)
test = subset(Boston, split==FALSE)

set.seed(1)

rf.boston = randomForest(medv~.,data = train, mtry = 6, importance=TRUE)
rf.boston
importance(rf.boston)
varImpPlot(rf.boston)
yhat.rf = predict(rf.boston,test)
mean((yhat.rf-test$medv)^2)

X = c()
Y = c()
for (i in 1:13) {
        rf.boston = randomForest(medv~., data = train, mtry = i)
        yhat.rf = predict(rf.boston,test)
        X[i] = mean((yhat.rf - test$medv)^2)
        Y[i] = i
}
mtryData = data.frame(X,Y)

mtryPlot = ggplot(mtryData, aes(x = Y, y = X)) + geom_line()
mtryPlot
# 6 seems to be the best value for mtry

# ___________________________________________________________________

data(Carseats)
split = sample.split(Carseats$Sales, 0.6)
train = subset(Carseats, split==TRUE)
test = subset(Carseats, split==FALSE)

tree.carseats = tree(Sales~., train)
tree.carseats
plot(tree.carseats)
text(tree.carseats, pretty = 0)
rpart.carseats = rpart(Sales~., train)
fancyRpartPlot(rpart.carseats)

yhat.rtree = predict(rpart.carseats, test)
mean((yhat.rtree - test$Sales)^2)
# MSE is 5.282.

fitControl = trainControl(method="cv", number = 10)
cartGrid = expand.grid(.cp=(1:50)*0.01)
train(Sales~., train, method="rpart", trControl = fitControl, tuneGrid = cartGrid)

rpart.carseats2 = rpart(Sales~., train, control = rpart.control(cp=0.01))
fancyRpartPlot(rpart.carseats2)
yhat.rtree2 = predict(rpart.carseats2, test)
mean((yhat.rtree2 - test$Sales)^2)

# MSE has remained the same. 

set.seed(121)
bag.carseats = randomForest(Sales~., train, mtry=10, importance=TRUE)
bag.predict = predict(bag.carseats, test)
mean((bag.predict - test$Sales)^2)
varImpPlot(bag.carseats, type=1)
# Bagging has reduced the MSE down quite considerably to 2.89

rf.carseats = randomForest(Sales ~., train, mtry=sqrt(3), importance=TRUE)
rf.predict = predict(rf.carseats, test)
mean((rf.predict - test$Sales)^2)
varImpPlot(rf.carseats, type=1)
# RF does not do as well as Baggin. Decorrelating the nodes had had a -ve effect on the test score. 


# ----------------------------------------------------------

data(OJ)

split = sample.split(OJ$Purchase, 0.75)
train = subset(OJ, split == TRUE)
test = subset(OJ, split == FALSE)

oj.tree = tree(Purchase ~., train)
summary(oj.tree)
# Training error rate is 0.1507. The tree has 8 terminal nodes. 

oj.tree
# 4) Splitting variable is LoyalCH. The splitting value is 0.278, and there are 177 values under this node. No star mean not terminal node. 

plot(oj.tree)
text(oj.tree)
# LoyalCH is the most important variable in this tree. 

yhat.oj = predict(oj.tree, test, type="clas")
head(yhat.oj)
head(test$Purchase)
table(yhat.oj, test$Purchase)
1 - (149+65)/(149+65+39+14)
# Test error rate is 19.85%. 

cv = cv.tree(oj.tree)
# Optimal size is with 6 terminal nodes. 
plot(cv$size, cv$dev, type="b")
prune.oj = prune.misclass(oj.tree, best=6)
summary(prune.oj)
# Same training error rate. 
plot(prune.oj)
text(prune.oj)
yhat.prune = predict(prune.oj, test, type="class")
table(yhat.prune, test$Purchase)
# Exactly the same error as un-prunned tree. 



# ________________________________________________________________________

data(Hitters)
colSums(is.na(Hitters))
Hitters = na.omit(Hitters)

Hitters$logSalary = log(Hitters$Salary)
head(Hitters)
dim(Hitters)

split = sample.split(Hitters$Salary, 0.75)
Hitters.train = subset(Hitters, split == TRUE)
Hitters.test = subset(Hitters, split == FALSE)

set.seed(103)
pows = seq(-10, -0.2, by=0.1)
lambdas = 10 ^ pows
length.lambdas = length(lambdas)
train.errors = rep(NA, length.lambdas)
test.errors = rep(NA, length.lambdas)
for (i in 1:length.lambdas) {
        boost.hitters = gbm(Salary~., data=Hitters.train, distribution="gaussian", n.trees=1000, shrinkage=lambdas[i])
        train.pred = predict(boost.hitters, Hitters.train, n.trees=1000)
        test.pred = predict(boost.hitters, Hitters.test, n.trees=1000)
        train.errors[i] = mean((Hitters.train$Salary - train.pred)^2)
        test.errors[i] = mean((Hitters.test$Salary - test.pred)^2)
}

plot(lambdas, train.errors, type="b", xlab="Shrinkage", ylab="Train MSE", col="blue", pch=20)

plot(lambdas, test.errors, type="b", xlab="Shrinkage", ylab="Test MSE", col="red", pch=20)
min(test.errors)
lambdas[which.min(test.errors)]

lm.fit = lm(Salary~., data=Hitters.train)
lm.pred = predict(lm.fit, Hitters.test)
mean((Hitters.test$Salary - lm.pred)^2)
library(glmnet)
set.seed(134)
x = model.matrix(Salary~., data=Hitters.train)
y = Hitters.train$Salary
x.test = model.matrix(Salary~., data=Hitters.test)
lasso.fit = glmnet(x, y, alpha=1)
lasso.pred = predict(lasso.fit, s=0.01, newx=x.test)
mean((Hitters.test$Salary - lasso.pred)^2)

summary(gbm(Salary~., data=Hitters.train, distribution="gaussian", n.trees=1000, shrinkage=lambdas[which.min(test.errors)]))


set.seed(21)
rf.hitters = randomForest(Salary~., data=Hitters.train, ntree=500, mtry=19)
rf.pred = predict(rf.hitters, Hitters.test)
mean((Hitters.test$Salary - rf.pred)^2)
# Test MSE is 631


# -----------------------------------------------------------------

data(Caravan)

dim(Caravan)

Caravan$Purchase = ifelse(Caravan$Purchase == "Yes", 1, 0)
table(Caravan$Purchase)


split = sample.split(Caravan$Purchase, 0.2)
train = subset(Caravan, split==TRUE)
test = subset(Caravan, split==FALSE)

boost.caravan = gbm(Purchase~., train, distribution="bernoulli", n.trees = 1000, shrinkage=0.01)
summary(boost.caravan)
# Variables MINK7512 and MBERHOOG seem to be the most important. 

yhat.prob = predict(boost.caravan, test, n.trees = 1000, type = "response")
head(yhat.prob)
pred = rep(0, nrow(test))
pred[yhat.prob > 0.2] = 1

table(pred, test$Purchase)
1 - (4226+36)/(4226+36+242+153)
# We have a test error rate of 0.08
36/(153+36)
# About 20% of people that were predicted to make a purchase do actually make it. 

glm1 = glm(Purchase ~., train, family="binomial")

glm.prob = predict(glm1, test, type="response")
head(glm.prob)
pred.glm = ifelse(glm.prob > 0.2, 1, 0)
table(pred.glm, test$Purchase)
(64)/(64+385)
# Considerably worse with GLM as it only gets 14% correct. 