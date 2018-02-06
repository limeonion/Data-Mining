# (10)
#-a)
library(MASS)
data(Boston)
?Boston
dim(Boston)
# 506 rows, 14 coloumns
# Rows represent observation, coloumns represent variables

#-b)
pairs(Boston)
# Strong correlation between lstat and medv (i.e median value of home decreases for lower status of population)
# Age of home is correlated with nox, higher proportion of older homes more nitrogen oxide

#-c)
# River dummy variable (chas) shows some correlation
# As does rad (accessibility to radial highway)

#-d)
head(Boston)
range(Boston$crim)
summary(Boston$crim)
range(Boston$crim)
# Virtually 0 to 88.98; very wide range: two orders of magnitude
range(Boston$tax)
# 187 to 711; not as wide of a range as crime rate
range(Boston$ptratio)
# 12.6 to 22.0; not as wide of a range as crime rate

#-e)
dim(subset(Boston, chas == 1))
# 35 suburbs

#-f)
median(Boston$ptratio)
# 19.05

#-g)
> t(subset(Boston, medv == min(Boston$medv)))
#              399      406
# crim     38.3518  67.9208 above 3rd quartile
# zn        0.0000   0.0000 at min
# indus    18.1000  18.1000 at 3rd quartile
# chas      0.0000   0.0000 not bounded by river
# nox       0.6930   0.6930 above 3rd quartile
# rm        5.4530   5.6830 below 1st quartile
# age     100.0000 100.0000 at max
# dis       1.4896   1.4254 below 1st quartile
# rad      24.0000  24.0000 at max
# tax     666.0000 666.0000 at 3rd quartile
# ptratio  20.2000  20.2000 at 3rd quartile
# black   396.9000 384.9700 at max; above 1st quartile
# lstat    30.5900  22.9800 above 3rd quartile
# medv      5.0000   5.0000 at min
summary(Boston)
# Not the best place to live, but certainly not the worst.

#-h)
dim(subset(Boston, rm > 7))
# 64
dim(subset(Boston, rm > 8))
# 13
summary(subset(Boston, rm > 8))
summary(Boston)
# relatively lower crime (comparing range), lower lstat (comparing range)