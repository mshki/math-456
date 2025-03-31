# Loading required R packages
library(tidyverse)
library(caret)
library(ggplot2)
theme_set(theme_bw())

# Load data set 
raisins <- read.csv("/Users/katherineshi/math456/essay_3/Raisin_Dataset.csv")
raisins <- na.omit(raisins)

# Split the data into training and test set
set.seed(37)
raisins <- sample(nrow(raisins))
split <- sample.split(raisins$Class, SplitRatio = 0.8)

# training.samples <- raisins$Class %>% 
  # createDataPartition(p = 0.8, list = False)

train = subset(raisins, split == TRUE)
test = subset(raisins, split == FALSE)

mylogit <- glm(Class ~ Area + MajorAxisLength + MinorAxisLength + Eccentricity + ConvexArea + Extent + Perimeter, data = train, family = "binomial")

summary(mylogit)
