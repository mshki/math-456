if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}

install.packages("tidyverse")
install.packages("caTools")
install.packages("ggcorrplot")



library(car)
library(lmtest)
library(tidyverse)
library(caTools)
library(ggcorrplot)

advertising <- read.csv("Advertising.csv")
str(advertising)
head(advertising, 4)

# Visualize variable distributions
for (col_name in names(advertising)) {
  if (is.numeric(advertising[[col_name]])) {
    hist(advertising[[col_name]],
         main = paste("Histogram of", col_name),
         xlab = col_name)
  }
}


# Find the outlier in newspaper
Q1 <- quantile(advertising$newspaper, 0.25)
Q3 <- quantile(advertising$newspaper, 0.75)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

outlier_indices <- which(advertising$newspaper < lower_bound | advertising$newspaper > upper_bound)

print(paste("Outlier value:", advertising$newspapers[outlier_indices]))
print(paste("Outlier indices:", outlier_indices))
advertising <- advertising[-outlier_indices, ]


# Train test split
set.seed(123)
split <- sample.split(advertising$sales, SplitRatio = 0.8)
train_set <- subset(advertising, split == TRUE)
test_set <- subset(advertising, split == FALSE)
dim(train_set)
dim(test_set)

model1 <- lm(sales ~ TV + radio + newspaper, data = train_set)
summary(model1)
summary(model1)$coefficients

model2 <- lm(sales ~ TV + radio, data = train_set)
summary(model2)
confint(model2)

# hist(residuals(model2), main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue", breaks = 20)
qqnorm(residuals(model2))
qqline(residuals(model2), col = "red", lwd = 2)

par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(model2)
par(mfrow = c(1, 1))  # Reset plot layout

dwtest(model2)
# sigma(model2)/mean(marketing$sales)

#find MSE on test set
predictions_model1 <- predict(model1, newdata = test_set)
predictions_model2 <- predict(model2, newdata = test_set)

mse_model1 <- mean((test_set$sales - predictions_model1)^2)
print(paste("MSE for model1:", mse_model1))

mse_model2 <- mean((test_set$sales - predictions_model2)^2)
print(paste("MSE for model2:", mse_model2))


#Cook's distance to remove influential points
cooksD <- cooks.distance(model2)
plot(cooksD,type="b",pch=18,col="red")
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
print("Indices of influential points:")
print(names(influential))

#correlation between predictors
reduced_data <- subset(advertising, select=-X)
corr_matrix = round(cor(reduced_data),2)

ggcorrplot(corr_matrix, hc.order = FALSE, type="lower", lab=TRUE)
