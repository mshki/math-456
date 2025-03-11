advertising <- read.csv("Advertising.csv")
str(advertising)
head(advertising, 4)
model1 <- lm(sales ~ TV + radio + newspaper, data = advertising)
summary(model1)
summary(model1)$coefficients
model2 <- lm(sales ~ TV + radio, data = advertising)
summary(model2)
confint(model2)
sigma(model2)/mean(marketing$sales)

