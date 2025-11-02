#set working directory
setwd("/cloud/project")
###Importing my original data set to RStudio and calling it MCE
MCE<-read.csv("Mall_Customers_extended.csv", header=TRUE)

#to view variable names
names(MCE)


#######  Step 1: Perform a K-Means Cluster analysis   #########
# I created MCEcluster1 data frame with the only 3 variables needed
# for my cluster: annual income, spending score, age

MCEcluster<-data.frame(MCE)

MCEcluster1<-MCEcluster[ ,c("Annual.Income..k..", "Spending.Score..1.100.", "Age")]

MCEcluster1

#Now, I'm going to create a new file called MCEsv with my 3 values standardized
#so that I can begin running my clusters
MCEcluster2<-data.frame(MCEcluster1)

MCEsv <- scale(MCEcluster2)

#View first 5 observations
head(MCEsv,n=5)

###### K-Means Algorithm #######################
#The factoextra package creates clusters in R studio 

install.packages("factoextra")
library(factoextra)

install.packages("rstatix")
library(rstatix)

#To find the number of clusters needed we use the fviz_nbclust function

fviz_nbclust(MCEsv, kmeans, method="wss") + geom_vline(xintercept = 0, linetype = 2)


## I'm using 4 clusters based on the graph in my plots box
fviz_nbclust(MCEsv, kmeans, method="wss") + geom_vline(xintercept = 4, linetype = 2)




#to obtain descriptive stats on 4 clusters
set.seed(123)

km.res <- kmeans(MCEsv, 4, nstart=25)
##Per homework instructions,  do not run line 52
print(km.res)


#####  Step 2: Create the Clustering Visual  ###############

#reating a new dataset using the cbind() function to merge the subset data 
#created in step 1, and the km.res$clsuter

dd <- cbind(MCEsv, cluster=km.res$cluster)

fviz_cluster(km.res, data=dd, 
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
             ellipse.type = "euclid", #Concentration ellipse
             star.plot = TRUE, #Add segments from centroids to items
             repel = TRUE, #Avoid label overplotting 
             ggtheme = theme_minimal()
             
#boxplot
# Step 1: Select columns for clustering
data_for_clustering <- MCE[, c("Age", "Annual.Income..k..", "Spending.Score..1.100.")]

# Step 2: Run k-means clustering 
set.seed(123)  # for reproducibility
km.res <- kmeans(data_for_clustering, centers = 3, nstart = 25)

# Step 3:  cluster assigned
km.res$cluster
# Merge cluster  with original data
MCE_clustered <- cbind(MCE, Cluster = km.res$cluster)
# Boxplot for Age across clusters
boxplot(Age ~ Cluster, data = MCE_clustered,
        main = "Distribution of Age Across Clusters",
        xlab = "Cluster", ylab = "Age",
        col = c("red", "green", "blue"))

# Boxplot for Annual Income across clusters
boxplot(Annual.Income..k.. ~ Cluster, data = MCE_clustered,
        main = "Distribution of Annual Income Across Clusters",
        xlab = "Cluster", ylab = "Annual Income (k$)",
        col = c("red", "green", "blue"))

# Boxplot for Spending Score across clusters
boxplot(Spending.Score..1.100. ~ Cluster, data = MCE_clustered,
        main = "Distribution of Spending Score Across Clusters",
        xlab = "Cluster", ylab = "Spending Score",
        col = c("red", "green", "blue"))
             
             