#(9)
data(Auto)

#-a)

pairs(Auto)

#-b)

cor(Auto[,-9])

#-c)

lm1 = lm(mpg~.-name, data = Auto)
summary(lm1)
# Given such a large F-statistic, this provides evidence of relationship between predictor and the response
# There is a significant relationship with displacement, weight, year and origin
# The older the car (model year) the higher the mpg

#-d)

plot(lm1)
plot(predict(lm1), rstudent(lm1))
# We see some non-linearity with the Residuals vs Fitted plot
# There are outliers and leverage points, data values greater than 3, and point 14!

#-e)
Auto2 = Auto[,1:8]
lm2 = lm(mpg~.*., data = Auto2)
summary(lm2)

# Displacement:Year, Accelration:Year, Acceleration:Origin are all significant at the 5% level. 

#(f)

plot(sqrt(Auto$horsepower), Auto$mpg, pch = 19, col ="blue")
plot((Auto$horsepower)^2, Auto$mpg, pch = 19, col ="blue")
plot(log(Auto$horsepower), Auto$mpg, pch = 19, col ="blue")

lm3 = lm(mpg~.+log(horsepower), data = Auto2)
summary(lm3)
plot(lm3)

# Similar problems, but better fit than lm1. More exploration needed. 