library(plyr)
library(animation)
wine <- read.csv(file.choose())
View(wine)
wdata<-wine[,-1]
attach(wdata)
cor(wdata)
pcaobj1<-princomp(wdata, cor = TRUE, scores = TRUE, covmat = NULL)

str(pcaobj1)
summary(pcaobj1)
loadings(pcaobj1)

plot(pcaobj1)

biplot(pcaobj1)
pcaobj1
# cbind used to bind the data in column wise
# Considering top 3 principal component scores and binding them with mydata
wdata<-cbind(wdata,pcaobj1$scores[,1:3])
View(wdata)


# Normalizing the data 
norm_data <- scale(wdata[,13:16])# Scale function is used to normalize data
fit <- kmeans(norm_data, 3) # 4 cluster solution
str(fit)
final2<- data.frame(wdata, fit$cluster) # append cluster membership
final2
final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
aggregate(wdata[,1:16], by=list(fit$cluster), FUN=mean)

twss = c()
for (i in 2:15) twss[i] = sum(kmeans(norm_data, centers=i)$withinss)
plot(1:15, twss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
View(twss)
plot(twss,type = "b")
km <- kmeans(wdata,3) #kmeans clustering
str(km)

km$withinss

# k clustering alternative for large dataset - Clustering Large Applications (Clara)
library(cluster)
xds <- rbind(cbind(rnorm(wdata), rnorm(wdata)), cbind(rnorm(wdata), rnorm(wdata)))
xcl <- clara(xds, 3, sample = 100)
clusplot(xcl)

write.csv(final3,file="WinesPCA_FINAL_kClust.csv",row.names = F, col.names = F)
getwd()
