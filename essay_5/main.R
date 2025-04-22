library(dplyr)
library(ggplot2)
library(caTools)
library(GGally)
library(factoextra)
library(caret)

df <- read.csv("data/SP500.csv")

# Clean and prep
df$Date <- as.Date(df$Date)
df <- na.omit(df)

# Create prev_close, prev_return feature
df <- df %>%
  group_by(Ticker) %>%
  arrange(Date) %>%
  mutate(prev_close = lag(Close),
         prev_return = lag(Returns)) %>%
  ungroup() %>%
  na.omit()

# Train/test split
set.seed(77)
split <- sample.split(df$Close, SplitRatio = 0.8)
train <- subset(df, split == TRUE)
test <- subset(df, split == FALSE)

# KMeans Clustering
## Select features for clustering
cluster_features <- train %>%
  select(Open, High, Low, Adjusted, Returns, Volume, prev_close)

## Scale features
scaled_train <- scale(cluster_features)

# Estimating Optimal Number of Clusters
## Elbow Method
fviz_nbclust(scaled_train, kmeans, method="wss")
geom_vline(xintercept=3, linetype=2)

### we find that there isn't an elbow, so we get the silhouette instead
### in order to find the optimal number of clusters

## Average Silhouette
fviz_nbclust(scaled_train, kmeans, method = "silhouette")
### this shows the optimal number of clusters as 2

## Gap Statistics
gap_stat <- clusGap(scaled_train,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)

fviz_gap_stat(gap_stat)
### calculating the gap statistics shows us
### the optimal number of clusters is 3

### to be on the safer side, we take optimal 
### number of clusters to be 3

## Fit KMeans
km.res <- kmeans(scaled_train, centers = 3)
km.res

train$cluster <- as.factor(km.res$cluster)

test_scaled <- scale(test %>% select(Open, High, Low, Adjusted, Returns, Volume, prev_close),
                     center = attr(scaled_train, "scaled:center"),
                     scale = attr(scaled_train, "scaled:scale"))

assign_cluster <- function(x, centers) {
  dists <- apply(centers, 1, function(c) sum((x - c)^2))
  return(which.min(dists))
}

test$cluster <- apply(test_scaled, 1, assign_cluster, centers = km.res$centers)
test$cluster <- as.factor(test$cluster)

# Predict return using clusters 
return_model <- lm(Returns ~ cluster + prev_return, data = train)
test$predicted_return <- predict(return_model, newdata = test)

# Plot: Actual vs Predicted Returns
test$index <- 1:nrow(test)
ggplot(test, aes(x = index)) +
  geom_line(aes(y = Returns, color = "Actual Return")) +
  geom_line(aes(y = predicted_return, color = "Predicted Return")) +
  ggtitle("Actual vs Predicted Returns (Test Set)") +
  ylab("Returns") +
  xlab("Row Index") +
  scale_color_manual(values = c("Actual Return" = "pink", "Predicted Return" = "slateblue")) +
  theme_minimal()

# Analyzing the model
summary(return_model) 

## Calculate prediction accuracy
data.frame(R2 = R2(test$predicted_return, test$Returns),  
           MSE = MAE(test$predicted_return, test$Returns))

# Diagonal Plots
plot_data <- test[, 2:8]
plot_data$Predicted <- test$predicted_return
ggpairs(plot_data, mapping = aes(y = Predicted, color = "pink"),
        columns = 2:8,
        title = "Diagonal Plot Colored by Predicted Returns (kMeans)")