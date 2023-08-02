library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)

mydata <- read_excel(path = "C:/Users/udaya/Desktop/Spring2023/cmsc462/homeworks/hw3/CovidMortality.xlsx")



mydata$Region <- setNames(state.region, state.name)[mydata$State]
mydata$deathRatio <- mydata$Deaths / mydata$Population

mydata$positive <- mydata$Confirmed / mydata$Population

multiple_regression <- lm(Deaths~Region+positive,mydata)
summary(multiple_regression)
