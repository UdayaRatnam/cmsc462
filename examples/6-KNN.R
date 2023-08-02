library(tidyverse)
library(readxl)
library(kknn)
setwd("C:/Users/User.DESKTOP-PM04AA1/Documents/UMBC/CourseDataSets")
 

## Regression with *knn*

ggplot(data = cars, mapping = aes(x = dist, y = speed)) +
geom_point(color = "#006EA1") +
labs(title = "Speed vs Stopping Distance",
x = "Stopping Distance", y = "Speed") + theme_light()


# formula is the same as in lm()
#Note that we are using the whole dataset for both train & test 
KNN_7 = kknn(speed ~ dist, train = cars, test = cars, k = 7)

#Compare with actual value
ggplot(data = cars, mapping = aes(x = dist, y = speed)) + geom_point(color = "#006EA1") +
geom_point(aes(x = dist, y = KNN_7$fitted.values), color = "orange", size = 2) +
labs(title = "Speed vs Stopping Distance",
x = "Stopping Distance", y = "Speed") + theme_light()


KNN_10 = kknn(speed ~ dist, train = cars, test = cars, k = 10)

ggplot(data = cars, mapping = aes(x = dist, y = speed)) + geom_point(color = "#006EA1") +
geom_point(aes(x = dist, y = KNN_10$fitted.values), color = "orange", size = 2) +
labs(title = "Speed vs Stopping Distance",
x = "Stopping Distance", y = "Speed") + theme_light()

KNNCarOptimal = train.kknn(speed ~ dist,data = cars, kmax = 20) # max k we are interested in trying

# Now we can view the results with the summary function
summary(KNNCarOptimal)

#Build a model that is optimal
KNN_4 = kknn(speed ~ dist, train = cars, test = cars, k = 4)

ggplot(data = cars, mapping = aes(x = dist, y = speed)) + geom_point(color = "#006EA1") +
  geom_point(aes(x = dist, y = KNN_4$fitted.values), color = "orange", size = 2) +
  labs(title = "Speed vs Stopping Distance",
       x = "Stopping Distance", y = "Speed") + theme_light()
sum((KNN_4$fitted.values -cars$speed)^2)
nrow(cars)

## Classification with *knn*
Heart = read_excel("Heart Disease.xlsx")


# Recode the HeartDisease as a factor
Heart$HeartDisease = factor(Heart$HeartDisease,
levels = c("Yes","No"), labels = c("Yes","No"))

# Let's try k = 6
KNNHeart = kknn(HeartDisease ~ ChestPain + Age, train = Heart, test = Heart, k = 6)

# Add predicted values to our dataset
Heart = Heart %>% mutate(HeartDiseaseKNN = KNNHeart$fitted.values)

# Let's take a look at the result
Heart %>% select(HeartDisease, HeartDiseaseKNN)

Heart %>% group_by(HeartDisease,HeartDiseaseKNN) %>% summarise(Patients = n())


# Optimal k
set.seed(12745) # This sets a start value for generating random numbers

KNNHeartOptimal = train.kknn(HeartDisease ~ ChestPain + Age,
data = Heart, kmax = 20) # max k we are interested in trying

# Now we can view the results with the summary function
summary(KNNHeartOptimal)



