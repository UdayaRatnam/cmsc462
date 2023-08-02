library(psych)
library(readxl)
library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(e1071)
library(tidyverse)
library(mlbench)
library(caret)
library(ROCR)
data <- read.csv("C:/Users/udaya/Desktop/Spring2023/cmsc462/homeworks/hw3/Lending.csv")
df <- data.frame(data)
#print(df$loan_default)
summary(data)
describe(data)

#gets a 3000 random 0's and 3000 random 1's from loan_default
sample <- rbind(filter(df, df$loan_default == 0) %>% sample_n(., 3000),filter(df, df$loan_default == 1) %>% sample_n(., 3000))
hist(sample$loan_default)

#Logistic Regression
log_reg = lm(sample$loan_default ~ sample$loan_amnt + sample$adjusted_annual_inc + sample$pct_loan_income + sample$dti + sample$residence_property + sample$months_since_first_credit + sample$inq_last_6mths + sample$open_acc + sample$bc_util + sample$num_accts_ever_120_pd + sample$pub_rec_bankruptcies)
summary(log_reg)
anova(log_reg)

columnsChanged = c("loan_default", "loan_amnt", "adjusted_annual_inc", "pct_loan_income", "dti","residence_property", "months_since_first_credit", "inq_last_6mths", "open_acc", "bc_util", "num_accts_ever_120_pd", "pub_rec_bankruptcies")
sapply(df[columnsChanged], unique)
df[columnsChanged]  = lapply(df[columnsChanged], as.factor)

smp_size <- floor(0.70 * nrow(df))


set.seed(123)
train_inded <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_inded, ]
test <- df[-train_inded, ]

#Naive Bayes
NVmodeled <- naiveBayes(loan_default ~ ., data = train)

preds <- predict(NVmodeled, newdata = test)
conf_matrix <- table(test$loan_default, preds)

predsRaws <- predict(NVmodeled, newdata = test, type = "raw")
predsRaws

NVmodeled$apriori


##########ROC STUFF########################
# Compute AUC for predicting Class with the model
prob <- predict(NVmodeled, newdata=test, type="raw")
pred <- prediction(prob[,2], test$loan_default)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
#following order: bottom, left, top, and right. 
par(mar=c(5,8,1,.5))
#Receiver operating characteristic
plot(perf, col="red")
abline(a=0, b=1)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

rocData = data.frame(c(perf@alpha.values, perf@x.values, perf@y.values))


