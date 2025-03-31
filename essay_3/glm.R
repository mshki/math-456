# Load required libraries
library(caTools)
library(tidyverse)
library(caret)
library(ggplot2)
theme_set(theme_bw())

# Load data set
raisins <- read.csv("Raisin_Dataset.csv")
raisins <- na.omit(raisins) # Remove missing values

# Convert Class to binary (Besni = 1, Kecimen = 0)
raisins$Class <- ifelse(raisins$Class == "Besni", 1, 0)

# Shuffle data
set.seed(37)
raisins <- raisins[sample(nrow(raisins)), ]

# Split into training and test set
split <- sample.split(raisins$Class, SplitRatio = 0.8)

train <- subset(raisins, split == TRUE)
test <- subset(raisins, split == FALSE)

# Logistic Regression Model
mylogit <- glm(Class ~ Area + MajorAxisLength + MinorAxisLength + 
                 Eccentricity + ConvexArea + Extent + Perimeter, 
               data = train, 
               family = "binomial")

summary(mylogit)