#########################
data(USArrests)
dim(USArrests)
head(USArrests)
scaled.us = scale(USArrests)
head(scaled.us)

a = dist(scaled.us^2)
b = as.dist(1-cor(t(scaled.us)))
summary(b/a)
#########################

#########################
library(dplyr)

apply(USArrests, 2, var)
apply(USArrests, 2, mean)

pr.out = prcomp(USArrests, center = TRUE, scale=TRUE)
pr.out
names(pr.out)
pr.out$sdev

pve = pr.out$sdev^2 / sum(pr.out$sdev^2)
pve


loadings = pr.out$rotation
pve2 = rep(NA, 4)
dmean = apply(USArrests, 2, mean)
dsdev = sqrt(apply(USArrests, 2, var))
dsc = sweep(USArrests, MARGIN=2, dmean, "-")
dsc = sweep(dsc, MARGIN=2, dsdev, "/")
for (i in 1:4) {
        proto_x = sweep(dsc, MARGIN=2, loadings[,i], "*")
        pc_x = apply(proto_x, 1, sum)
        pve2[i] = sum(pc_x^2)
}
pve2 = pve2/sum(dsc^2)
pve2
##########################


##########################

hc = hclust(dist(USArrests), method="complete")
plot(hc)
cutree(hc, 3)
# This shows which states belong to which cluster. 
table(cutree(hc,3))

# Scaled version

hc.s = hclust(dist(scaled.us), method="complete")
plot(hc.s)
cutree(hc.s, 3)
table(cutree(hc.s, 3), cutree(hc,3))

# Scaling the variable does affect the results you get after you cut the tree. This is because scaling the variables affects the heights of the branches. 
###########################


###########################
dim(dat)

dd = as.dist(1 - cor(dat))
plot(hclust(dd, method="complete"))
plot(hclust(dd, method="single"))
plot(hclust(dd, method="average"))
# Depending on the linkage method, it seperates it into 2 or 3 groups. 


pr.out = prcomp(t(dat))
summary(pr.out)
pr.out$rotation
total_load = apply(pr.out$rotation, 1, sum)
total_load
indices = order(abs(total_load), decreasing=T)
indices
indices[1:10]
total_load[indices[1:10]]
