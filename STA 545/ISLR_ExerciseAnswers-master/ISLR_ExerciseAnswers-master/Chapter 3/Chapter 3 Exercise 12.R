# (12)

#-a)

# When the sum of xi^2 = sum yi^2

#-b)

x = rnorm(100)
y = 3*x + rnorm(100)

sum(x^2)
sum(y^2)

#-c)

x = 1:100
y = x + rnorm(100, 0.00001)


sum(x^2)
sum(y^2)
