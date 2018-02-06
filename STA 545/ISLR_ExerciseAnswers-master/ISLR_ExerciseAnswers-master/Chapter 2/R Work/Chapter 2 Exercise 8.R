#(8)
#-a)
college = read.csv("College.csv")

#-b)
fix(college)
rownames(college) = college[,1]
college = college[,-1]
fix(college)

#-c)
#i.
summary(college)
#ii.
pairs(college[,1:10])
#iii.
boxplot(college$Outstate, college$Private)
#vi.
Elite = rep("No", nrow(college))
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college,Elite)

summary(college)
# There are 78 elite universities

boxplot(college$Outstate, college$Elite)

#v and vi.
par(mfrow=c(1,1))
plot(college$Outstate, college$Grad.Rate)
# High tuition correlates to high graduation rate.
plot(college$Accept / college$Apps, college$S.F.Ratio)
# Colleges with low acceptance rate tend to have low S:F ratio.
plot(college$Top10perc, college$Grad.Rate)
# Colleges with the most students from top 10% perc don't necessarily have
# the highest graduation rate. Also, rate > 100 is erroneous!











