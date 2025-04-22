# libraries
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

# data preparation
idk <- read.csv("idk")
df <- scale(idk)

# estimating optimal number of clusters
## elbow method 
fviz_nbclust(df, kmeans, method="wss")
geom_vline(xintercept=3, linetype=2)

## average silhouette
fviz_nbclust(df, kmeans, method = "silhouette")

# k-means clustering
set.seed(37)
km.res <- kmeans(df, 4, nstart=25)
print(km.res)

## mean of each feature
aggregate(idk, by=list(cluster=km.res$cluster), mean)

dd <- cbind(idk, cluster = km.res$cluster)
head(dd)

## cluster stats for each observation
km.res$cluster # cluster number
km.res$size # cluster size
km.res$centers # cluster means

# Visualize
fviz_cluster(km.res, df[, -5],
             palette = "Set2", ggtheme = theme_minimal())

