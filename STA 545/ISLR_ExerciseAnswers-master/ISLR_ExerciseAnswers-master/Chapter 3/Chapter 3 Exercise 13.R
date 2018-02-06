# (13)

#-a)

set.seed(1)
x = rnorm(100, 0, 1) 

#-b)

eps = rnorm(100, 0, 0.25)

#-c) 

y = -1 + 0.5*x + eps

summary(y)
length(y)

# len = 100, b0 = -1, b1 = 0.5

#-d)
plot(x,y)
# Strong linear relationship (correllation)

#-e)
lm1 = lm(y~x)
summary(lm1)
# b0 and b1 are very close to their original values

#-f)
abline(lm1)
abline(-1, 0.5, col="red")

#-g)

lm2 = lm(y~poly(x, 2))
summary(lm2)
# Regression coefficient for the 2nd term is insignificant