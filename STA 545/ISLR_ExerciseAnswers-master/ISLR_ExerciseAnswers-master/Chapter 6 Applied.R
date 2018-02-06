library(leaps)
library(glmnet)
library(ISLR)
library(caTools)
library(pls)


x = rnorm(100)
eps = rnorm(100)
y = 1 + 2*x + 3*x^2 + 4*x^3 + eps
data1 = data.frame(x = x, y = y)
head(data1)

reg.fit = regsubsets(y ~ poly(x, 10), data1, nvmax = 10)
reg.summary = summary(reg.fit)
names(reg.summary)
reg.summary$rsq
# Based on the rsq, the best model is the one that contains 10 variables
reg.summary$cp
# The best model according to Mallows Cp is the one that contains 3 variables
which.min(reg.summary$bic)
# The best model according to BIC is the one that contains 3 variables 
par(mfrow= c(2,2))
plot(reg.summary$cp, type = "l")
plot(reg.summary$bic, type = "l")
plot(reg.summary$rsq, type = "l")
plot(reg.fit, scale = "Cp")
plot(reg.fit, scale = "r2")
plot(reg.fit, scale = "bic")


reg.fitFwd = regsubsets(y ~ poly(x, 10), data1, nvmax = 10, method = "forward")
reg.fitBwd = regsubsets(y ~ poly(x, 10), data1, nvmax = 10, method = "backward")
summary(reg.fitFwd)
plot(reg.fitFwd, scale = "Cp")
plot(reg.fitFwd, scale = "r2")
plot(reg.fitFwd, scale = "bic")
# The 3 variable model once again seems to be the one chosen by Mallows CP and BIC

summary(reg.fitBwd)
plot(reg.fitBwd, scale = "Cp")
plot(reg.fitBwd, scale = "r2")
plot(reg.fitBwd, scale = "bic")
# The 3 variable model once again seems to be the one chosen by Mallows CP and BIC


# Lasso
X = model.matrix(y~poly(x,10), data1)[,-1]
# Matrix with no intercept
head(X)
Y = data1$y
head(Y)
# Creatig a grid of values to be used for Lambda's
grid = 10^seq(10, -2, length=100)
mod.lambda = cv.glmnet(X, Y, alpha=1)
mod.lambda$lambda.min
best.lambda = mod.lambda$lambda.1se
# The best lambda value within 1sd of the minimum is 0.26
lasso.mod = glmnet(X,Y,alpha=1,lambda=grid)
dim(coef(lasso.mod))
coef(lasso.mod)[,50]
coef(lasso.mod)[,99]
lasso.mod$lambda[99]
predict(lasso.mod, s = best.lambda, type="coefficients")
# Best model again is the one with the first 3 variables

y = 1 + 7*x^7 + eps
data2 = as.data.frame(x=x, y=y)

reg.fit = regsubsets(y ~ poly(x, 10), data2, nvmax = 10)
reg.summary = summary(reg.fit)
plot(reg.fit, scale = "Cp")
plot(reg.fit, scale = "r2")
plot(reg.fit, scale = "bic")
# The first 7 variable model is the best

X = model.matrix(y~poly(x,10), data2)[,-1]
Y = data1$y
lambda.mod = cv.glmnet(X,Y,alpha=1)
best.lambda = lambda.mod$lambda.1se
best.model = glmnet(X,Y,alpha=1)
predict(best.model, s=best.lambda, type="coefficients")
# Lasso seems to pick the first 3 variables as the best model



data(College)
head(College)

split = sample.split(College$Apps, 0.7)
train = subset(College, split == TRUE)
test = subset(College, split == FALSE)

lm1 = lm(Apps ~., train)
summary(lm1)

prediction = predict(lm1, test)
head(prediction)
mse = mean((test$Apps - prediction)^2)
mse
# A simple linear model gives us a rss error of 1420214

X.train = model.matrix(Apps~., train)[,-1]
X.test = model.matrix(Apps~., test)[,-1]
Y.train = train$Apps
grid = 10 ^ seq(10, -2, length=100)

mod.ridge = cv.glmnet(X.train, Y.train, alpha = 0, lambda=grid)
best.lambda = mod.ridge$lambda.1se
best.lambda
ridge.pred = predict(mod.ridge, newx=X.test, s=best.lambda)
ridge.mse = mean((test$Apps - ridge.pred)^2)
ridge.mse
# MSE higher than OLS, surprising. 


mod.lasso = cv.glmnet(X.train, Y.train, alpha=1, lambda=grid)
best.lambda = mod.lasso$lambda.1se
best.lambda
lasso.pred = predict(mod.lasso, newx=X.test, s=best.lambda)
lasso.mse = mean((test$Apps - lasso.pred)^2)
lasso.mse
# MSE higher than OLS but lower than Ridge. 
# The coefficients are: 
predict(mod.lasso, s=best.lambda, type="coefficients")


pcr.fit = pcr(Apps~., data=train, scale=TRUE, validation ="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
pcr.pred = predict(pcr.fit, test, ncomp=9)
pcr.mse = mean((pcr.pred - test$Apps)^2)
pcr.mse
# The MSE for PCR is 4107436, which is higher than all the above models tried. 

pls.fit = plsr(Apps~., data=train, scale=TRUE, validation ="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
pls.pred = predict(pls.fit, test, ncomp=9)
pls.mse = mean((pls.pred - test$Apps)^2)
pls.mse
# The MSE for PLS is 1824386, which is the 2nd lowest. 

# The linear model and pLS seem to give the best results. 