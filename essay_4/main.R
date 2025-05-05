# Load required libraries
library("Cairo")
library("class")
library("caret")
library("rpart")
library("rpart.plot")
library("ggplot2")
library("tree")
library("randomForest")
library("GGally")

# --- Load and preprocess the data ---
raisins <- read.csv("data/Raisin_Dataset.csv")
raisins <- na.omit(raisins)  # Remove missing values

# Convert Class to binary: Besni = 1, Kecimen = 0
raisins$Class <- factor(raisins$Class, levels = c("Kecimen", "Besni"))
#raisins$Class <- ifelse(raisins$Class == "Besni", 1, 0)

# --- Normalize numeric features for KNN ---
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
raisins_norm <- as.data.frame(lapply(raisins[1:7], normalize))
raisins_labels <- raisins$Class
raisins_norm$Class <- as.factor(raisins_labels)  # caret expects factors

# --- Feature selection using RFE (Recursive Feature Elimination) ---
set.seed(456)

# Define RFE control
rfe_ctrl <- rfeControl(functions = rfFuncs,  # Use random forest for ranking
                       method = "cv", 
                       number = 10)

# Run RFE
rfe_result <- rfe(x = raisins_norm[, 1:7],
                  y = raisins_norm$Class,
                  sizes = c(1:7),
                  rfeControl = rfe_ctrl)

# Show selected features
print(rfe_result)
selected_features <- predictors(rfe_result)
cat("Selected Features:", selected_features, "\n")

# --- Cross-validation to find the best K using caret ---
set.seed(11)
train_control <- trainControl(method = "cv", number = 10)  # 10-fold CV

# Train KNN model with internal CV
knn_cv_model <- train(Class ~ ., 
                      data = raisins_norm, 
                      method = "knn", 
                      trControl = train_control, 
                      tuneLength = 20)  # tries k = 1 to 20

# Output best model and k
print(knn_cv_model)
CairoPNG("figures/cv_plot.png")
plot(knn_cv_model)
dev.off()

# Get the best K value
best_k <- knn_cv_model$bestTune$k
cat("Best K:", best_k, "\n")

# --- Split dataset into train and test for evaluation using best K ---
set.seed(456)
train_idx <- sample(1:nrow(raisins), size = 0.7 * nrow(raisins))
train_data <- raisins_norm[train_idx, 1:7]  # only features
test_data  <- raisins_norm[-train_idx, 1:7]
train_labels <- raisins_norm$Class[train_idx]
test_labels  <- raisins_norm$Class[-train_idx]

# Predict with best K from cross-validation
knn_final_pred <- knn(train = train_data, test = test_data, cl = train_labels, k = best_k)

# --- Evaluate performance ---
confusionMatrix(knn_final_pred, test_labels, mode = "everything")

# --- Visualize predictions ---
plot_data <- test_data
plot_data$Predicted <- knn_final_pred
CairoPNG("figures/knn_pairs_plot.png", width = 1200, height = 1200)
ggpairs(plot_data, mapping = aes(color = Predicted),
        columns = 1:7, title = "Diagonal Plot Colored by Predicted Class (kNN)")
dev.off()

#  Decision Tree model for comparison ---
#raisins$Class <- factor(raisins$Class, levels = c(0, 1))
# --- Cross-validation for Decision Tree ---
set.seed(123)
tree_cv_model <- train(Class ~ ., 
                       data = raisins,
                       method = "rpart",
                       trControl = train_control,  # already defined as 10-fold CV
                       tuneLength = 10)  # explore different complexity parameters (cp)

# Output the best model and its complexity parameter
print(tree_cv_model)
CairoPNG("figures/tree_cv_plot.png", width = 800, height = 600)
plot(tree_cv_model)
dev.off()

# Use the best tree model to make predictions on the test set
tree_best_model <- tree_cv_model$finalModel

print("%%%%%% DECISION TREE %%%%%%")
CairoPNG("figures/tree_plot.png", width = 800, height = 600)
rpart.plot(tree_best_model)
dev.off()
tree_pred <- predict(tree_best_model, raisins[-train_idx,], type = "vector")
conf_matrix_tree <- table(Predicted = round(tree_pred), Actual = test_labels)
accuracy <- sum(diag(conf_matrix_tree))/sum(conf_matrix_tree)
print(conf_matrix_tree)
cat("Accuracy of Tree Model:", accuracy, "\n")
