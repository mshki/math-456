# Load libraries
library(showtext) # For rendering text 
library(caTools) # For data manipulation 
library(tidyverse)
library(caret) # For classification training 
library(ggplot2) # For creating graphs
library(dplyr) # For dataframe manipulation
library(pROC) # For displaying and analyzing ROC curves
library(ggfortify) # For plotting GLM 
theme_set(theme_bw())
showtext_auto()

# Load data set 
raisins <- read.csv("Raisin_Dataset.csv")
raisins <- na.omit(raisins) # Remove missing values

# Convert Class to binary (Besni = 1, Kecimen = 0)
raisins$Class <- ifelse(raisins$Class == "Besni", 1, 0)

# Train-test split
set.seed(37)
raisins <- raisins[sample(nrow(raisins)), ]
split <- sample.split(raisins$Class, SplitRatio = 0.8)
train <- subset(raisins, split == TRUE)
test <- subset(raisins, split == FALSE)

# Fit model
model <- glm(Class ~ Area + MajorAxisLength + MinorAxisLength + 
                 Eccentricity + ConvexArea + Extent + Perimeter, 
               data = train, 
               family = "binomial")
summary(model)
autoplot(model)

# Predictions
pred <- predict(model, test, type = "response")
pred_class <- ifelse(pred > 0.5, "Besni", "Kecimen")

# Confusion Matrix
conf_matrix <- table(Predicted = pred_class, Actual = test$Class)
print(conf_matrix)

# Logistic Regression Model
train.data <- train %>%
  mutate(prob = predict(model, type = "response"))

area_plot <- ggplot(train.data, aes(x = Area, y = Class)) +
  geom_point(alpha = 0.3) +  
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model for Area",
    x = "Raisin Area",
    y = "Probability of being Besni"
  )
area_plot

ecc_plot <- ggplot(train.data, aes(x = Eccentricity, y = Class)) +
  geom_point(alpha = 0.3) +  
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model for Eccentricity",
    x = "Eccentricity",
    y = "Probability of being Besni"
  )
ecc_plot

# ROC curve
roc_curve <- roc(test$Class, pred)
plot(roc_curve, col = "cadetblue", main = "ROC Curve")
auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")

# Feature Importance
importance <- summary(model)$coefficients[, "Estimate"]
importance_df <- data.frame(Feature = names(importance), Estimate = importance)
ggplot(importance_df, aes(x = reorder(Feature, Estimate), y = Estimate)) +
  geom_bar(stat = "identity", fill = "pink") +
  coord_flip() +
  labs(title = "Feature Importance",
       x = "Feature",
       y = "Coefficient Estimate")


