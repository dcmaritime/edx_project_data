#Model Description and Model Results

#This bit of code seeks to find an elbow in the data which is indicative of an optimal number of cluster for the k-means algorithm.  It iterates enough times storing the cluster values and then plots the clusters for viewing.


mydata <- read.csv("C:/Users/dclar/Dropbox/edX/Capstone/Own_Project/Data/whnotype.csv")
df <- data.frame(mydata)

df_z <- as.data.frame(lapply(df,scale))


wss <- (nrow(df_z)-1)*sum(apply(df_z,2,var))
for (i in 2:25) wss[i] <- sum(kmeans(df_z,centers=i,iter.max=25)$withinss)
plot(1:25, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")



#This code creates the R dataframe for the modeling process and runs some summary statistics on the modeling data, as well as correlations between the variables in the data. It also reads in container index values that will be inserted into the model report, so that you will be able to see which container indexes belong to which clusters.


mydata <- read.csv("C:/Users/dclar/Dropbox/edX/Capstone/Own_Project/Data/whnotype.csv")
CI <- read.csv("C:/Users/dclar/Dropbox/edX/Capstone/Own_Project/Data/container_index.csv")

df <- data.frame(mydata)

head(df)

summary(df)

Cor1<-cor(df$Query_Count,df$Unique_IP_Count)
Cor1

Cor2<-cor(df$Query_Count,df$Destination_Count)
Cor2

Cor3<-cor(df$Query_Count,df$Vessel_Count)
Cor3

Cor4<-cor(df$Unique_IP_Count,df$Destination_Count)
Cor4

Cor5<-cor(df$Unique_IP_Count,df$Vessel_Count)
Cor5

Cor6<-cor(df$Destination_Count,df$Vessel_Count)
Cor6


#This code standardizes the data, sets a seed value in order to be able to recreate the results and then runs the k-means algorithm.

#The following k-means modeling parameters are set: 9 clusters, max iterations = 25 and random set with which to start = 25.  In some cases the k-means algorithm failed due to convergence being beyond the default max iterations.

#It creates a K-means report for evaluating the cluster results by container index.  This report will be used to search for actual groundtruth records. Cluster results are called and serveral graphs are creating that might help understand the data distribution.



df_z <- as.data.frame(lapply(df,scale))

set.seed(3)

wh.out=kmeans(df_z,9,iter.max=25,nstart=20)

WH_Kmeans_Report_plot <- cbind(wh.out$cluster,df_z)

WH_Kmeans_Report <- cbind(CI,wh.out$cluster,df_z)

setwd("C:/Users/dclar/Dropbox/edX/Capstone/Own_Project/Data")

write.csv(WH_Kmeans_Report, file='WH_K-means_Report.csv',quote=F, row.names=F)

wh.out$centers

wh.out$totss

wh.out$withinss

wh.out$tot.withinss

wh.out$betweenss

wh.out$size

#wh.out$iter

#wh.out$ifault

library(ggplot2)
ggplot(data=WH_Kmeans_Report_plot,aes(Query_Count,Unique_IP_Count,color=wh.out$clusters))+geom_point()
ggplot(data=WH_Kmeans_Report_plot,aes(Query_Count,Destination_Count,color=wh.out$clusters))+geom_point()
ggplot(data=WH_Kmeans_Report_plot,aes(Query_Count,Vessel_Count,color=wh.out$clusters))+geom_point()


library(lattice)
attach(WH_Kmeans_Report_plot)
cloud(Vessel_Count~Query_Count*Unique_IP_Count,data=WH_Kmeans_Report_plot,screen = list(x = 50, y = 20))
```


