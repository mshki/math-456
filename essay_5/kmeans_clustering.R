# Libraries
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(ggplot2)
library(dplyr)
library(caTools)

# Data Preparation
data <- read.csv("data/SP500.csv")
data <- na.omit(data)

## Data Cleaning
df <- data[, -which(names(data) == "Date")]
df <- df[, -which(names(df) == "Ticker")]

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

# Historical vs. Predicted Closing Price Using Cluster-Based LM

## Calculate prev_close
df$cluster <- km.res$cluster

pred <- df %>%
  mutate(prev_close = lag(Close)) %>%
  na.omit()

split <- sample.split(pred$cluster, SplitRatio = 0.8)
train <- subset(pred, split == TRUE)
test <- subset(pred, split == FALSE)

model <- lm(Close ~ cluster + prev_close, data = train)

## Predict next day's close
test$predicted_close <- predict(model, newdata = test)

## Plot Actual v Predicted 
test$index <- 1:nrow(test)

ggplot(test, aes(x = index)) +
  geom_line(aes(y = Close, color = "Actual")) +
  geom_line(aes(y = predicted_close, color = "Predicted")) +
  ggtitle("Closing Price Prediction (by day)") +
  ylab("Price") +
  xlab("Day") +
  scale_color_manual(values = c("Actual" = "slateblue", "Predicted" = "pink")) +
  theme_minimal()