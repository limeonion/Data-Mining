library(ISLR)
data(Auto)

lm1 = lm(mpg ~ horsepower, Auto)
summary(lm1)

# Given such a low p-value, we can reject the null hypothesis and assume that there is a relationship# between the
# horsepower and mpg. 
# With a r-squared value of 0.6, just over 60% of the variation of mpg is explained by the horepower variable.
# This is a pretty decent relationship strength wise. There is a positive relationship between mpg and horsepower. 

predict(lm1, data.frame(horsepower = 98))
predict(lm1, data.frame(horsepower = 98), interval = "confidence")
predict(lm1, data.frame(horsepower = 98), interval = "prediction")

plot(Auto$horsepower, Auto$mpg)
abline()
par(mfrow=c(2,2))
plot(lm1)

# Looking at the Residuals vs Fitted plot we see a clear U-shape, which provides strong indication of non-linearity in the data. 
# Looking at the QQ-plot, there seems to be some evidence of skewness (right-skewed).
# Residuals vs Leverage plot demonstrates some high leverage points in the data, as well as some outliers. 



pairs(Auto)
cor(Auto[,-9])
lm2 = lm(mpg ~ ., Auto[, -9])
summary(lm2)

# Looking at the Residuals vs Fitted plot we see some evidence of non-linearity, due to the U-shape present. 
# There also seems to be outliers and a high leverage points (point 14 especially has a high leverage). 

# With a high F-statistic and a very small p-value, we can conclude that there is a relationship between the predictors and response
# Displacement, weight, year and origin have a statistically significant relationship with mpg amongst the predictors
# The coeffecient for the year variable suggests that mpg increases as the year increases

par(mfrow=c(2,2))
plot(lm2)

# Using the correlation matrix, I have chosen those variables with the highest correlation between them for interaction. 
lm3 = lm(mpg~cylinders*displacement+displacement*weight, Auto[,-9])
summary(lm3)
# Interaction between displacement and weight is significant, whilst between cylinders and desplacement is not. 

lm4 = lm(mpg ~ log(weight) + sqrt(horsepower) + acceleration + I(acceleration^2), Auto[,-9])
summary(lm4)
plot(lm4)

# Acceleration and horsepower have become significant after their transformation, when compared to original model. 
# All of the transformed variables above are siginificant. 
# There is less evidence of non-linearity when looking at the Residuals vs Fitted plot. However there are still high 
# leverage points, as well as outliers present. 

data(Carseats)
cs = Carseats

cslm = lm(Sales ~ Price + Urban + US, cs)
summary(cslm)
# Price is a significant variable and has a coefficient of -0.0545. This means that as the price increases, then umber of sales decrease. 
# The Urban variable is not significant. The coefficient UrbanYes is negative, meaning if the store is in an Urban area, then sales will decrease. 
# The US variable is significant, meaning there is a relationship between the store being in the US or not and the Sales variable. 
# USYes has a coeffcient value of 1.2, meaning that sales increase if the stores is in the US by approximately 1200 units. 

# We can reject the null hypothesis for Price and US variables. 

cslm2 = lm(Sales ~ Price + US, cs)
summary(cslm2)

# According to the adjusted R-squared, both models fit the data relatively well. he 2nd one is slightly better, and has less predictors
# so idealy this is the one we should use. 

confint(cslm2, level=0.95)
plot(predict(cslm2), rstudent(cslm2))
plot(cslm2)
# Looking at the Residuals vs Leverage plot, there are some evidence of a high leverage point. 

set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100)
datas = data.frame(y,x)
lm = lm(y ~ x + 0, datas)
summary(lm)
# There is a significant relationship between x and y. 
lm2 = lm(x ~ y + 0, datas)
summary(lm2)
# There is a significant relationship between y and x. 
# Both represent the same line y = 2x + e

lmt1 = lm(y ~ x, datas)
lmt2 = lm(x ~ y, datas)
summary(lmt1)
summary(lmt2)
# As you can see both coeffiecients for beta1 is the same. 

set.seed(1)
x = rnorm(100)
y = x + rnorm(100)

lm.fit1 = lm(y~x+0)
lm.fit2 = lm(x~y+0)
summary(lm.fit1)
summary(lm.fit2)

x <- rnorm(100)
y <- -sample(x, 100)
head(x)
head(y)
sum(x^2)
sum(y^2)
lm.fit <- lm(y~x+0)
lm.fit2 <- lm(x~y+0)
summary(lm.fit)
summary(lm.fit2)
# When the sum of x^2 and sum of y^2 are the same, then their coefficients are the same in a linear model with no intercept. 


set.seed(1)
x = rnorm(100, 0, 1)
eps = rnorm(100,0.25)
y = -1+0.5*x+eps
plot(x,y)
# There is some positive relationship between x and y. 
lm1 = lm(y~x)
summary(lm1)
# bo is off by about 0.22 and b1 is very close.
abline(lm1)
lm2 = lm(y ~ x + I(x^2))
summary(lm2)
# It improves it slightly according to the r^2
confint(lm1, level = 0.95)
