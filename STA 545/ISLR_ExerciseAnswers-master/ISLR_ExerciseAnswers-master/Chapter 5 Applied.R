library(boot)
library(MASS)
library(ISLR)

data(Default)

set.seed(12345)
glmB = glm(default ~ income + balance, Default, family="binomial")

train1 = Default[1:6000, ]
test1 = Default[6001:10000, ]
train2 = Default[4001:10000, ]
test2 = Default[1:4000, ]

glm1 = glm(default ~ income + balance, train1, family = "binomial")
glm1.prob = predict(glm1, test1, type="response")
glm1.pred = rep("No", nrow(test1))
glm1.pred[glm1.prob > 0.5] = "Yes"
table(glm1.pred, test1$default)
1 - (3852+44)/(3852+44+90+14)
# Error rate of 2.6%


glm2 = glm(default ~ income + balance, train2, family = "binomial")
glm1.prob2 = predict(glm2, test2, type="response")
glm1.pred2 = rep("No", nrow(test2))
glm1.pred2[glm1.prob2 > 0.5] = "Yes"
table(glm1.pred2, test2$default)
1 - (3848+41)/(3848+41+97+14)
# Error rate of 2.7% 
# We are getting slightly different error rate depending on the split we are using!

glm = glm(default ~ income + balance + student, train1, family = "binomial")
glm.prob = predict(glm, test1, type="response")
glm.pred = rep("No", nrow(test1))
glm.pred[glm.prob > 0.5] = "Yes"
table(glm.pred, test1$default)
1 - (3848+42)/(3848+42+92+18)
# This has increased the error by 0.1% to 2.7%! 


summary(glmB)
# SE for income is 0.000004985 and for balance is 0.0002274


boot.fn = function(Default, index){
  
  glm10 = glm(default ~ income + balance, Default, subset = index, family = "binomial")
  glm10$coefficients[2:3]
  
}
# The SE is lower to what glm gives. 


data(Weekly)
glmW = glm(Direction ~ Lag1 + Lag2, Weekly, family="binomial")
glmW2 = glm(Direction ~ Lag1 + Lag2, Weekly[-1,], family="binomial")
glmW2.prob = predict(glmW2, Weekly[1,], type="response")
names(glmW2.prob)
glmW2.prob
glmW2.pred = rep("Down", nrow(Weekly[1,]))
glmW2.pred[glmW2.prob > 0.5] = "Up"
table(glmW2.pred, Weekly[1,]$Direction)
# Predicted up, but the market is really going down on that day.

outcome = rep(0, nrow(Weekly))
for (i in 1:nrow(Weekly)) {
  glm = glm(Direction ~ Lag1 + Lag2, Weekly[-i,], family = "binomial")
  glm.prob = predict(glm, Weekly[i,], type = "response")
  glm.pred = rep("Down", nrow(Weekly[i,]))
  glm.pred[glm.prob > 0.5] = "Up"
  if (glm.pred != Weekly[i,]$Direction)
    outcome[i] = 1
}

# LOOCV estimates a test error rate of 45%

set.seed(1)
y = rnorm(100)
x=rnorm(100)
y = x-2*x^2+rnorm(100)
# n = 100, p = 1 (although the squared term may be considered another tranformed predictor?) making p = 2

plot(x,y)
# There us heavy curvuture to the plot

set.seed(999)
x 
y
data = data.frame(x,y)
data
lm1 = glm(y ~ x, data = data)
lm2 = glm( y ~ x + poly(x,2), data = data)
lm3 = glm( y ~ x + poly(x,2) + poly(x,3), data = data)
lm4 = glm( y ~ x + poly(x,2) + poly(x,3) + poly(x,4), data = data)

cv.err1 = cv.glm(data, lm1)
cv.err2 = cv.glm(data, lm2)
cv.err3 = cv.glm(data, lm3)
cv.err4 = cv.glm(data, lm4)

cv.err1$delta
cv.err2$delta
cv.err3$delta
cv.err4$delta

# Exact same, as it evaluates n fold on a single observation
# The quadratic one had the smallest one, as this matches the true Y

summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)
# p-value shows significance of the linear and polynomial terms, whcih agree with CV results, as lm2 had the lowest test error. 

data(Boston)
mean(medv)
sd(medv)
sd(medv)/sqrt(nrow(Boston))

boot.fn = function(data, index){
  return(mean(data[index]))
}

bstrap = boot(medv, boot.fn, 1000)
bstrap
# The SE is very close to the one I originally calculated.

t.test(Boston$medv)
names(bstrap)
bstrap.CI = c(bstrap$t0 - 1.96*0.3931208, bstrap$t0 + 1.96*0.3931208)
bstrap.CI
# The confidence intervals are almost exactly the same. 


median(medv)
boot.fn = function(data, index){
  return(median(data[index]))
}

bstrapm = boot(medv, boot.fn, 1000)
bstrapm
# Small SE relative to median. 

medv.tenth = quantile(medv, c(0.1))
medv.tenth

boot.fn = function(data, index) return(quantile(data[index], c(0.1)))
boot(medv, boot.fn, 1000)
# Again small SE relative to 10th percentile value. 

