library("fpc")
library("diptest")
library("cluster")
library("stats")


clusterData<-read.csv("F:/R_WorkingSpace/courseWork/DataCombine.csv")
reutersForCluster <- read.csv("F:/R_WorkingSpace/courseWork/reutersDeleteRows.csv")

clusterData<-clusterData[-which(reutersForCluster$purpose == "not-used"),]

#The distance based clustering
model.kmeans<-kmeans(clusterData,10)
model.kmeans$size
[1]  704  460  565  488 2306 1373  474  730 1019 1688
#Get the distributions of those 10 most popular attributes
ten.col<-c("topic.earn", "topic.acq", 
           "topic.money.fx", "topic.grain", 
           "topic.crude", "topic.trade", 
           "topic.interest", "topic.ship", 
           "topic.wheat", "topic.corn")
attr.list<-c(0)
for( i in 1:10){
  the_total<-sum(reutersForCluster[,which((colnames(reutersForCluster) == ten.col[i]) == T)])
  attr.list<-c(attr.list,the_total)
}
attr.list<-attr.list[-1]
kmeansRes<-model.kmeans$size[order(model.kmeans$size,decreasing = T)]

kmeansRes
[1] 2306 1688 1373 1019  730  704  565  488  474  460
attr.list
[1] 3759 2213  688  580  573  517  426  301  288  224
#easy to see that the the distribution are not very similar to each other


#Hierrarchicall clustering
dm<-dist(clusterData,method = "euclidean")
fitmodel<-hclust(dm,method = "average")
cop<-cophenetic(fitmodel)
cor(cop,dm)
[1] 0.9531326
#Mavollous!

#Density Based Ckustering
model.db <- dbscan(clusterData,0.5,5,scale=T,showplot=T,method="raw")
model.db
