#Including Libraries
library(plyr)
library(animation)
#Importing and Editing the Data for the algorithm
wine <- read.csv(file.choose())
View(wine)
wdata<-wine[,-1]
attach(wdata)
cor(wdata)
#Applying the principle Component Analysis Algorithm 
pcaobj1<-princomp(wdata, cor = TRUE, scores = TRUE, covmat = NULL)

#Visualising the data set after passing through the algorithm
str(pcaobj1)
summary(pcaobj1)
loadings(pcaobj1)
plot(pcaobj1)
biplot(pcaobj1)

#Viewing the output
#pcaObj$loadingspcaobj
pcaobj1$scores[,1:4]# Top 3 PCA Scores which represents the whole data
# cbind used to bind the data in column wise
# Considering top 3 principal component scores and binding them with mydata
wdata<-cbind(wdata,pcaobj$scores[,1:3])
View(wdata)

# preparing data for clustering (considering only pca scores as they represent the entire data)
clus_data<-wdata[,8:10]

# Normalizing the data 
norm_clus<-scale(clus_data) # Scale function is used to normalize data
dist1<-dist(norm_clus,method = "euclidean") # method for finding the distance
# here I am considering Euclidean distance

# Clustering the data using hclust function --> Hierarchical
fit1<-hclust(dist1,method="complete") # method here is complete linkage

plot(fit1) # Displaying Dendrogram
plot(fit1,hang = -1)
groups<-cutree(fit1,3) # Cutting the dendrogram for 5 clusters
rect.hclust(fit1, k=3, border="red")
types<-as.matrix(groups) # cluster numbering 

View(types)

final1<-cbind(types,wdata) # binding column wise with orginal data
View(final1)
View(aggregate(final1[,-c(2,9:11)],by=list(types),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1

write.csv(final1,file="WinesPCA_FINAL_HClust.csv",row.names = F, col.names = F)
getwd()
