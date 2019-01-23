
#MachineLearning Lab1 (9 Jan 2019)
#Author:"Saurabh Kumar Singh"


data=read.csv("C:/Users/imsau/Desktop/6th Sem/ML/ML_lab/Lab1(9-Jan)/Wholesale customers data.csv")
View(data)

#3.	Observe the data ( in R-through summary and str)

summary(data)



#4.	We'll need to drop the Channel and Region variables. 
    #These are two ID fields and are not useful in clustering. So drop it.

cols.dont.want <- c("Channel", "Region")
data1 <- data[, ! names(data) %in% cols.dont.want, drop = F]

#5.Set some SEED value
#6.Apply the k-mean on dataset, with k=5


set.seed(0)
km<-kmeans(data1, 5)



#11.	Measure total SSE
km$totss

#10.	Measure homogeneity of each cluster (SSE)
km$withinss

km$tot.withinss
#12.	Measure the heterogeneity of cluster 
km$betweenss

km$iter

#13.	Elbow measure: run the algorithm 100 time for k=2 to 20. 
wss = kmeans(data1, centers=1)$tot.withinss
wss

for (i in 2:20)
  wss[i] = kmeans(data1, centers=i)$tot.withinss

#elbow plot
library(ggvis)
sse = data.frame(c(1:20), c(wss))
names(sse)[1] = 'Clusters'
names(sse)[2] = 'SSE'
sse %>%
  ggvis(~Clusters, ~SSE) %>%
  layer_points(fill := 'blue') %>% 
  layer_lines()
  

#Plots

library(animation)
kmeans.ani(data1, 5)



library(cluster)
library(fpc)
km$cluster
clusplot(data1, km$cluster, color=T, shade=F,labels=0,lines=0, main='k-Means Cluster Analysis')
