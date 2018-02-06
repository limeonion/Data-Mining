# (10)

data(Carseats)

#-a)
lm1 = lm(Sales ~ Price + Urban + US, data = Carseats)
summary(lm1)

#-b)

# Price - As sale increases by one unit, price goes down by 5.4p
# Urban - If the store is an Urban area, sales go down by -0.02 units (no relationship though)
# US - If store is in the US, sales increase by 1.2 units

#-c)
# Sales = -0.05*Price - 0.02*UrbanYes + 1.2*USYes

#-d) 
# Price and USYes, and F-Statistic

#-e)
lm2 = lm(Sales ~ Price + US, data = Carseats)

#-f)
summary(lm1)
summary(lm2)
# Both models have a very similar fit, with the sam r^2 value. 

#-g)
confint(lm2)

#-h)
par(mfrow=c(2,2))
plot(lm2)
plot(predict(lm2), rstudent(lm2))

# No signs of outliers
# Few high leverage point