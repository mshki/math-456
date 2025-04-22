# Libraries
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(ggplot2)
library(dplyr)

# Data Preparation
data <- read.csv("data/SP500.csv")

## Data Cleaning
df <- data[, -which(names(data) == "Date")]
df <- df[, -which(names(df) == "Ticker")]
df <- na.omit(df)

## Scale Values
scaled <- scale(df)

# Estimating Optimal Number of Clusters
## Elbow Method
fviz_nbclust(scaled, kmeans, method="wss")
geom_vline(xintercept=3, linetype=2)

### we find that there isn't an elbow, so we get the silhouette instead
### in order to find the optimal number of clusters

## Average Silhouette
fviz_nbclust(scaled, kmeans, method = "silhouette")
### this shows the optimal number of clusters as 2

## Gap Statistics
gap_stat <- clusGap(scaled,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)

fviz_gap_stat(gap_stat)
### calculating the gap statistics shows us
### the optimal number of clusters is 3

### to be on the safer side, we take optimal 
### number of clusters to be 3

# K-Means Clustering
set.seed(37)
km.res <- kmeans(df, 3, nstart=25)
km.res

## mean of each feature
aggregate(scaled, by=list(cluster=km.res$cluster), mean)

dd <- cbind(scaled, cluster = km.res$cluster)
head(dd)

## cluster stats for each observation
km.res$cluster # cluster number
km.res$size # cluster size
km.res$centers # cluster means

# Visualize
fviz_cluster(km.res, scaled[, -5],
             palette = "Set2", ggtheme = theme_minimal())
