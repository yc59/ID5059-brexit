library(car)
library(caret)
library(corrplot)
library(randomForest)
library(cowplot)
library(party)
library(gam)
library(nnet)

# TODO - Path should be properly declared
data <- read.csv("~/Desktop/ID5059/Practical02/ID5059-P2/master-copy.csv")

# TODO - Possibly not attach the data to global environment
# See https://stackoverflow.com/questions/10067680/why-is-it-not-advisable-to-use-attach-in-r-and-what-should-i-use-instead
attach(data)
summary(data)
str(data)
table(data$Party)

# Data Partition
set.seed(1234)
pd <- sample(2, nrow(data), replace = TRUE, prob = c(0.8, 0.2))
train <- data[pd == 1, ]
validate <- data[pd == 2, ]

## Multinomial Logistic Regression
mlr <- multinom(Party ~ B + D + H + J + K + L + M + O, data = train)
summary(mlr)
predict(mlr, validate)

#Misclassification Error for test data
tab5 <- table(predict(mlr), train$Party)
print(tab5)
1-sum(diag(tab5))/sum(tab5)

#Misclassification for validate data
testPred3 <- predict(mlr, newdata = validate)
tab6 <- table(testPred3, validate$Party)
print(tab6)
1-sum(diag(tab6))/sum(tab6)