#install package tidyverse if necessary
library(tidyverse)
setwd("C:/Users/User.DESKTOP-PM04AA1/Documents/UMBC/CourseDataSets")


advertising = read.csv("Advertising.csv")

ggplot(data = advertising, mapping = aes(x = TV, y =  Sales)) +
  geom_point(color = "red") + ylim(c(0,30)) + # set limits for y-axis for better viewing
  labs(title = "Sales vs. TV", x = "TV", y = "Sales") + theme_light()

ggplot(data = advertising, mapping = aes(x = Radio, y =  Sales)) +
  geom_point(color = "blue") + ylim(c(0,30)) + # set limits for y-axis for better viewing
  labs(title = "Sales vs. Radio", x = "Radio", y = "Sales" ) + theme_light()

ggplot(data = advertising, mapping = aes(x = Newspaper, y =  Sales)) +
  geom_point(color = "black") + ylim(c(0,30)) + # set limits for y-axis for better viewing
  labs(title = "Sales vs. Newspaper", x = "Newspaper", y = "Sales") + theme_light()

# Fit the linear regression model and assign it to "MyModel"
MyModel = lm(Sales ~ ., data = advertising)

# Coefficient Estimates, t statistics, p-values, R-squared, 
# and residual standard error
summary(MyModel)

## ANOVA (Analysis of Variance) Table
anova(MyModel)

## Obtaining the Parameter Estimates
coef(MyModel)

## 95% Confidence Intervals for the Parameters
confint(MyModel, level = 0.95)



myFittedValue = predict(MyModel,
        newdata = data.frame(TV = advertising$TV, Radio= advertising$Radio, Newspaper= advertising$Newspaper), interval = "confidence")

myNewData = data.frame(myFittedValue, Sales= advertising$Sales)

mySSR = sum((myNewData$fit - myNewData$Sales)^2)
mySSR
  