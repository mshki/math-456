library("caret")
library("rpart")
library("rpart.plot")
library("class")
library("ggplot2")
library("tree")
library("randomForest")

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

tree_model  <-  tree(Class ~ .,data=train_data)

summary(tree_model)
