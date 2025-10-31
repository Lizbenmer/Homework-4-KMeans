#set working directory
setwd("/cloud/project")

data("USArrests")

#to view variable names
names(USArrests)



#######################################
## Long way to Standardize Variables ##
#######################################

#if wanted to convert every variables data into a z-score
#murder
averageMurder <- mean(USArrests$Murder)
standarddevMurder <- sd(USArrests$Murder)
USArrests$Murder_standardized <- (USArrests$Murder - averageMurder) / standarddevMurder


#assault
averageAssault <- mean(USArrests$Assault)
standarddevAssault <- sd(USArrests$Assault)
USArrests$Assault_standardized <- (USArrests$Assault - averageAssault) / standarddevAssault


#Rape
averageRape <- mean(USArrests$Rape)
standarddevRape <- sd(USArrests$Rape)
USArrests$Rape_standardized <- (USArrests$Rape - averageRape) / standarddevRape

#UrbanPop
averageUrbanPop <- mean(USArrests$UrbanPop)
standarddevUrbanPop <- sd(USArrests$UrbanPop)
USArrests$UrbanPop_standardized <- (USArrests$UrbanPop - averageUrbanPop) / standarddevUrbanPop



#Using the scale command to standardize instead
arrestdata$standardMurder <- scale(arrestdata$Murder)

#assault
arrestdata$standardAssault <- scale(arrestdata$Assault)

#Rape
arrestdata$standardRape <- scale(arrestdata$Rape)

#UrbanPop
arrestdata$standardUrbanPop <- scale(arrestdata$UrbanPop)

names(arrestdata)


#######################################################
## Short way to Standardize All Continuous Variables ##
########################################################

#to standardize all the variables in our data in one step 
#provided that all the variables are continuous
df <- data.frame(USArrests)

df2 <- scale(df)




#View first 5 observations
head(df2,n=5)






install.packages("factoextra")
library(factoextra)

fviz_nbclust(df2, kmeans, method="wss") + geom_vline(xintercept = 0, linetype = 2)

fviz_nbclust(df2, kmeans, method="wss") + geom_vline(xintercept = 4, linetype = 2)




#to obtain descriptive stats on 4 clusters
set.seed(123)

km.res <- kmeans(df2, 4, nstart=25)

print(km.res)


########################################################
#another way to obtain average of each variable stratified by cluster
########################################################
aggregate(USArrests, by=list(cluster=km.res$cluster), mean)

#another way to obtain descriptive information
km.res$cluster

km.res$size

km.res$centers

#merge in cluster number
dd <- cbind(USArrests, cluster=km.res$cluster)




fviz_cluster(km.res, data=dd, 
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
             ellipse.type = "euclid", #Concentration ellipse
             star.plot = TRUE, #Add segments from centroids to items
             repel = TRUE, #Avoid label overplotting 
             ggtheme = theme_minimal()
)




##############################
##    K-Means - Part 2 #######
## Visualizing our Clusters ##
##############################


#create descriptive tables for each cluster
cluster1 <- subset(dd, dd$cluster==1)

cluster2 <- subset(dd, dd$cluster==2)

cluster3 <- subset(dd, dd$cluster==3)

cluster4 <- subset(dd, dd$cluster==4)





#Creating a boxplot for each variable by cluster provided by cluster datasets:

#####################################################
### Comparing Murder across clusters via boxplots ###
#####################################################

#using all clusters
is.factor(dd$cluster)

dd$cluster <- as.factor(dd$cluster)

is.factor(dd$cluster)

#want to see murder distribution  by cluster 
library(ggplot2)

#to find min credit card limit
min(dd$Murder)
max(dd$Murder)


#Boxplot
bar <- ggplot(dd, aes(cluster, Murder))

bar + geom_boxplot() + labs(x="Cluster", y="Murder Rates") +
  scale_y_continuous(limits = c(0, 20), breaks = seq(from =0, to = 20, by = 1)) + 
  ggtitle("Murder Distribution by Cluster ")



#######################################################
### Comparing Assaults across clusters via boxplots ###
#######################################################

#using all clusters
is.factor(dd$cluster)


#want to see murder distribution  by cluster 
library(ggplot2)

#to find min and max of Assault rates
min(dd$Assault)
max(dd$Assault)


#Boxplot
bar <- ggplot(dd, aes(cluster, Assault))

bar + geom_boxplot() + labs(x="Cluster", y="Assault Rates") +
  scale_y_continuous(limits = c(0, 400), breaks = seq(from =0, to = 400, by = 10)) + 
  ggtitle("Assault Distribution by Cluster ")



###################################################
### Comparing Rape across clusters via boxplots ###
###################################################

is.factor(dd$cluster)


#want to see credit card limits  by gender contained in cluster 1 data
library(ggplot2)

#to find min and max of Rape rates
min(dd$Rape)
max(dd$Rape)


#Boxplot
bar3 <- ggplot(dd, aes(cluster, Rape))

bar3 + geom_boxplot() + labs(x="Cluster", y="Rape") +
  scale_y_continuous(limits = c(0, 50), breaks = seq(from =0, to = 50, by = 1)) +
  ggtitle("Rape Distribution by Cluster")




###################################################
### Comparing UrbanPop across clusters via boxplots ###
###################################################

is.factor(dd$cluster)


library(ggplot2)

#to find min and max of Urban Pop Numbers
min(dd$UrbanPop)
max(dd$UrbanPop)


#Boxplot
bar4 <- ggplot(dd, aes(cluster, UrbanPop))

bar4 + geom_boxplot() + labs(x="Cluster", y="UrbanPop") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(from =0, to = 100, by = 5)) +
  ggtitle("UrbanPop Distribution by Clusters")





### Creating descriptive statistics for each cluster

install.packages("stargazer")
library(stargazer)

stargazer(cluster1, type="text", title="Descriptive Statistics for Cluster 1", digits=1, out="table_Cluster1.txt")

stargazer(cluster2, type="text", title="Descriptive Statistics for Cluster 2", digits=1, out="table_Cluster2.txt")

stargazer(cluster3, type="text", title="Descriptive Statistics for Cluster 3", digits=1, out="table_Cluster3.txt")

stargazer(cluster4, type="text", title="Descriptive Statistics for Cluster 4", digits=1, out="table_Cluster4.txt")




