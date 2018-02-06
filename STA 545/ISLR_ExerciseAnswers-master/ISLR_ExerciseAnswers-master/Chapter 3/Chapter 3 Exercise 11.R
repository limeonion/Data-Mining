# (11)

set.seed(1)
x = rnorm(100)
y = 2*x+rnorm(100)

#-a)
lm1 = lm(y~x+0)
summary(lm1)

# x is significant, t value is large and SE is small

#-b)

lm2 = lm(y~x)
summary(lm2)

# P-value is still significant for x, for the interecept it is not. Indicating
# model does not require a p-value. 

#-c)
# Both x's have significant coefficients. The R^2 values also went down by including the intercept. 
# This is due to it allowing for a zero output when a 0 inout is put in. 

#-e)
# When you do a regression on X onto Y, you just replace the Xs in the 
# previous equation with Ys and you will find that the equation remains 
# unchanged. Therefore, what you are essentially proving here is that there 
# is a correlation between x and y, but we can’t be sure of causation.

#-f)

x<- rnorm(100)
y<-2*x+rnorm(100)
lmC<-lm(y~x)
lmD<-lm(x~y)
summary(lmC)
summary(lmD)
