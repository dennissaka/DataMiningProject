#Data Preparing
# 1. Loading 
setwd("~/Desktop/DataMining Project")
my_data <- read.csv("Freedman.csv")

# 2. Set column X (names) as rownames
row.names(my_data) <- my_data$X
my_data[1] <- NULL

# 3. Remove any missing value (i.e, NA values for not available)
my_data <- na.omit(my_data)

# 4. Scale variables
my_data <- scale(my_data)

#Get library
library("cluster")
library("factoextra")

#Partitioning Clustering with K-means and PAM
#Determine the optimal number of clusters
fviz_nbclust(my_data, kmeans, method = "gap_stat")

#Compute and visualize k-means clustering
km.res <- kmeans(my_data, 9, nstart = 25)
fviz_cluster(km.res, data = my_data,frame.type = "convex")+
  theme_minimal()

# Compute and visualize PAM clustering
library("cluster")
pam.res <- pam(my_data, 9)
fviz_cluster(pam.res)


#Hirarchical clustering 
# Compute dissimilarity matrix
d <- dist(my_data, method = "euclidean")

# Hierarchical clustering using Ward's method
res.hc <- hclust(d, method = "ward.D2" )

# Cut tree into 9 groups
grp <- cutree(res.hc, k = 9)

# Visualize
plot(res.hc, cex = 0.6) # plot tree
rect.hclust(res.hc, k = 9, border = 2:5) # add rectangle

