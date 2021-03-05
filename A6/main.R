#Practical Assignment 6 
#Joshgun Guliyev - 2581347
#Anar Amirli - 2581604

#loading data to environment 
load("A6Data1.RData")

#performing PCA on data1 
pca = prcomp(data1,scale = TRUE)

#plotting data on first 2 principal components
PCs <- as.matrix(pca$x)
col.labs <- rep(c("Green", "Blue", "Red"))
plot(PCs[, 1], PCs[, 2], col=col.labs, pch=19, xlab = "Scores on PC1", ylab="Scores on PC2")

#performing k-means clustering on data1 with K = 3
km.out =kmeans(data1,3,nstart = 20)
clusters = km.out$cluster

#checking the accuracy of the clustering with K=3
accuracy_table_3 = table(data1_labels,clusters)
accuracy_table_3
#performing k-means clustering on data1 with K = 2
km.out_2 =kmeans(data1,2,nstart = 20)
clusters_2 = km.out_2$cluster
#checking the accuracy of the clustering with K=2
accuracy_table_2 = table(data1_labels,clusters_2)
accuracy_table_2

#performing k-means clustering on data1 with K = 4
km.out_4 =kmeans(data1,4,nstart = 20)
clusters_4 = km.out_4$cluster
#checking the accuracy of the clustering with K=4
accuracy_table_4 = table(data1_labels,clusters_4)
accuracy_table_4

#Performing clustering on first 2 principial components with K=3 

components = PCs[,1:2]

km.out_pcs =kmeans(components,3,nstart = 20)
clusters_pcs = km.out_pcs$cluster

#checking the accuracy of the clustering with K=3 on PC1 and PC2
accuracy_table_pcs = table(data1_labels,clusters_pcs)
accuracy_table_pcs

#Loading second data and plotting
load("A6Data2.Rdata")
plot(data)

#Ecludian distances sbetween observations
distance = dist(data)

#performing hirerical clustering based on three linkage method
hc.complete = hclust(distance, method = "complete")
hc.average = hclust(distance, method = "average")
hc.single = hclust(distance, method = "single")

#plotting 
par(mfrow = c(1,3))
plot(hc.complete, main = "Coplete Linkage",xlab = "",sub = "",cex = 0.9)
plot(hc.average, main = "Average Linkage",xlab = "",sub = "",cex = 0.9)
plot(hc.single, main = "Single Linkage",xlab = "",sub = "",cex = 0.9)

#Determining the number of clusters as 4 
cutree(hc.complete, 4)
cutree(hc.average, 4)
cutree(hc.single, 4)

#Determining the number of clusters as 3
cutree(hc.complete, 3)
cutree(hc.single, 3)
cutree(hc.average, 3)


