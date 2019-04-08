EIA<-read.csv(file="/Users/zhangliang/Desktop/data.csv", header=TRUE)
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

library(e1071)
# Notice the independence assumption is not true in most cases.
# Nevertheless, the system still performs incredibly well. 
# One strength of Naïve Bayes is that it is highly scalable 
# and can learn incrementally—all 
# we have to do is count the observed variables 
# and update the probability distribution.
# Can handle both categorical and numeric input variables, 
# but output must be categorical
naive <- naiveBayes(Party ~ B + D + H + J + K + L + M + O, data = train,laplace = 0)

# prediction  
prediction.naive <- predict(naive, validate)
table(prediction.naive,validate$Party)


#Misclassification for train data, there may exist some error and it is 
# reasonable to drop this part
train.naive.table<-table(prediction.naive,train$Party)
print(train.naive.table)
1-sum(diag(train.naive.table))/sum(train.naive.table)

#Misclassification for validate data
validate.naive <- predict(naive, newdata = validate)
validate.naive.table <- table(validate.naive, validate$Party)
print(validate.naive.table)
1-sum(diag(validate.naive.table))/sum(validate.naive.table)


