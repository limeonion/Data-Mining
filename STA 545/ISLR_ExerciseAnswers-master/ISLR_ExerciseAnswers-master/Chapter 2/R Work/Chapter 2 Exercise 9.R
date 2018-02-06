# (9)
#-a)
library(ISLR)
data(Auto)
?Auto
na.omit(Auto)
dim(Auto)
str(Auto)
# Appart from 'name', all variables are of a numerical structure, hence quantitative.

#-b)
range(Auto$cylinders)
range(Auto$mpg)
range(Auto$displacement)
range(Auto$horsepower)
range(Auto$weight)
range(Auto$acceleration)
range(Auto$year)
range(Auto$origin)

MeanStd = function(x = c(),...) {
        m = mean(x)
        s = sd(x)
        c = c(m,s)
        c
}
# Function to calculate both mean and standard deviation

MeanStd(Auto$mpg)
MeanStd(Auto$cylinders)
MeanStd(Auto$displacement)
MeanStd(Auto$horsepower)
MeanStd(Auto$weight)
MeanStd(Auto$acceleration)
MeanStd(Auto$year)
MeanStd(Auto$origin)

#-d)
Auto = Auto[-c(10:85),]

range(Auto$cylinders)
range(Auto$mpg)
range(Auto$displacement)
range(Auto$horsepower)
range(Auto$weight)
range(Auto$acceleration)
range(Auto$year)
range(Auto$origin)

MeanStd(Auto$mpg)
MeanStd(Auto$cylinders)
MeanStd(Auto$displacement)
MeanStd(Auto$horsepower)
MeanStd(Auto$weight)
MeanStd(Auto$acceleration)
MeanStd(Auto$year)
MeanStd(Auto$origin)


#-e)
data(Auto)
dim(Auto)
pairs(Auto)
# Seems like a strong +ive correlation between horsepower and weight
# Seems like a strong -ve correlation between mpg and horsepower
# Interesting to see Year relationship between Year of manufacture and MPG, and Horsepower

#-f)
# Yes, displacement, weight, horsepower, year are all correlated with mpg. 
# These correlations suggest good predictors to use
