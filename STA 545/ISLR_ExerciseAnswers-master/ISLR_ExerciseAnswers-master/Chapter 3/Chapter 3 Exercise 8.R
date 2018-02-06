#(8)
#-a)
data(auto)
lm1 = lm(mpg~horsepower, data = Auto)
summary(lm1)

#i. Yes
#ii. Very strong relationship, p-value < 0.001
#iii. Negative
#iv. 
predict(lm1, data.frame(horsepower=c(98)))
# 24.46708
predict(lm1, data.frame(horsepower=c(98)), interval = "confidence")
#        fit      lwr      upr
# 1 24.46708 23.97308 24.96108
predict(lm1, data.frame(horsepower=c(98)), interval = "prediction")
# fit     lwr      upr
# 1 24.46708 14.8094 34.12476

#-b)
plot(Auto$horsepower, Auto$mpg)
abline(lm1)

#-c) 
plot(lm1)
# Residual vs Fitted, non-linearity