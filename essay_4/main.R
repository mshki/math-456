library("class")
library("caret")
library("rpart")
library("rpart.plot")
library("class")
library("ggplot2")
library("tree")
library("randomForest")
library("GGally")

# Load data set
raisins <- read.csv("data/Raisin_Dataset.csv")
raisins <- na.omit(raisins) # Remove missing values

# Convert Class to binary (Besni = 1, Kecimen = 0)
raisins$Class <- ifelse(raisins$Class == "Besni", 1, 0)

# Shuffle data
set.seed(456)
train_idx  <- sample(1:nrow(raisins),size = 0.7 * nrow(raisins))
train_data  <- raisins[train_idx,]
test_data  <-  raisins[-train_idx,]

tree_model  <-  rpart(Class ~ .,data=train_data)

#tree_pred  <- predict(tree_model, data, type = "class")

summary(tree_model)
rpart.plot(tree_model)

# Normalize the features (KNN performs better with scaled data)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

raisins_norm <- as.data.frame(lapply(raisins[1:7], normalize))
raisins_labels <- raisins$Class

train_data <- raisins_norm[train_idx, ]
test_data <- raisins_norm[-train_idx, ]
train_labels <- raisins_labels[train_idx]
test_labels <- raisins_labels[-train_idx]

# Print the results
print(rfe_result)
plot(rfe_result)

train_subset <- train_data[, c("MajorAxisLength", "Extent", "Eccentricity", "Area", "ConvexArea")]
test_subset <- test_data[, c("MajorAxisLength", "Extent", "Eccentricity", "Area", "ConvexArea")]
knn_pred  <- knn(train = train_data, test = test_data, cl = train_labels,k=1)

summary(knn_pred)

confusionMatrix(knn_pred, as.factor(test_labels))

train_idx  <- sample(1:nrow(raisins),size = 0.7 * nrow(raisins))
train_data  <- raisins[train_idx,]
test_data  <-  raisins[-train_idx,]

tree_model  <-  rpart(Class ~ .,data=train_data)

tree_pred  <- predict(tree_model, test_data, type = "vector")

confusion_matrix <- table(Predicted = tree_pred, Actual = as.factor(test_labels))
print(confusion_matrix)

## --- Create DataFrames for Plotting ---
#knn_df <- data.frame(train_data, Predicted = knn_pred)
#tree_df <- data.frame(features, Predicted = tree_pred)  # unscaled for readability
#
## --- Pair Plot for k-NN Output ---
#ggpairs(knn_df, mapping = aes(color = Predicted), 
#        columns = 1:4, title = "k-NN Classification (Pair Plot)")
#
## --- Pair Plot for Decision Tree Output ---
#ggpairs(tree_df, mapping = aes(color = Predicted), 
#        columns = 1:4, title = "Decision Tree Classification (Pair Plot)")
#
#

plot_data <- test_data[, 1:7]
plot_data$Predicted <- knn_pred
ggpairs(plot_data, mapping = aes(color = Predicted),
        columns = 1:7,   # only the features in the plot
        title = "Diagonal Plot Colored by Predicted Class (kNN)")

