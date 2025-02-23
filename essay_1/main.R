# Load required packages
library(tidyverse) # For data manipulation and visualization
library(ggpubr) # Creates publication ready-plot
library(caTools) # For data analysis and manipulation tasks
library(lmtest) # For testing the model 
library(caret) # For regression training
theme_set(theme_classic())

# Load the dataset
scores <- read.csv("./dataset/Student_Marks.csv")
colnames(scores) # Print column names

# Visualization, pre-linear regression
scatter_plot <- ggplot(scores, aes(x = time_study, y = Marks)) +
  geom_point() +
  ggtitle("Scatter Plot") + 
  theme(plot.title = element_text(hjust = 0.5))
scatter_plot

# Split the dataset into test set and training set
set.seed(37)
split = sample.split(scores$time_study, SplitRatio = 0.8)
training_set = subset(scores, split == TRUE)
test_set = subset(scores, split == FALSE)

# Print the size of test set and training set
dim(training_set)
dim(test_set)

# Creating the model
model <- lm(Marks ~ time_study, data = training_set)

# Visualization of training set
scatter_plot <- ggplot(training_set, aes(x = time_study, y = Marks)) +
  geom_point() +
  stat_smooth(method = lm, col = "slateblue", lwd=1.5) +
  ggtitle("Training Set with Regression Line") + 
  theme(plot.title = element_text(hjust = 0.5))
scatter_plot

# Visualization of test set
scatter_plot <- ggplot(test_set, aes(x = time_study, y = Marks)) +
  geom_point() +
  stat_smooth(method = lm, col = "mediumaquamarine", lwd=1.5) +
  ggtitle("Test Set with Regression Line") +
  theme(plot.title = element_text(hjust = 0.5))
scatter_plot

# Analyzing the model
summary(model) # returns all summary statistics

sigma(model) # returns RSE

cor(training_set$time_study, training_set$Marks) # returns correlation coefficient

# Calculate prediction accuracy
pred <- predict(model, newdata=test_set)
data.frame(R2 = R2(pred, test_set$Marks),  
           MSE = MAE(pred, test_set$Marks))

# Set up Diagnostic Plots
# par(mfrow=c(2,1))

# 1 - Residuals vs Fitted Plot (Check linearity)
plot(model, 1)

# 2 - Q-Qplot (Check normality)
plot(model, 2)

# 3 - Scale-Location Plot (Check Homogeneity)
plot(model, 3)

# 4 - Cook's Distance (Identify Influential Points)
plot(model, 4)

# 5 - Residuals vs Leverage Plot (Check Outliers and High Leverage Points)
plot(model, 5)

# 2: Independence Check: Durbin-Watson Test
dwtest(model) # p-value > 0.05 suggests no autocorrelation

# 4: Normality check: Q-Q Plot and Shapiro-Wilk Test
shapiro.test(residuals(model)) # p-value > 0.05 suggests normality

# 5: Check Residuals Histogram
hist(residuals(model), main = "Histogram of Residuals", col = "lightblue")


