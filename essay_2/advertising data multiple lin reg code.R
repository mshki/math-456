if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}

install.packages("tidyverse")
install.packages("caTools")

library(car)
library(lmtest)
library(tidyverse)
library(caTools)

advertising <- read.csv("Advertising.csv")
str(advertising)
head(advertising, 4)

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

