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

## Decision Tree

tree <- ctree(Party ~ B + D + H + J + K + L + M + O, data = train,
              controls = ctree_control(mincriterion = 0.99, minsplit = 100))
                    #### Check the best mincriterion and minsplit??
tree
plot(tree)

#Predict
predict(tree, validate)

#Misclassification for train data
tab3 <- table(predict(tree), train$Party)
print(tab3)
1-sum(diag(tab3))/sum(tab3)

#Misclassification for validate data
testPred2 <- predict(tree, newdata = validate)
tab4 <- table(testPred2, validate$Party)
print(tab4)
1-sum(diag(tab4))/sum(tab4)

    © 2019 GitHub, Inc.
    Terms
    Privacy
    Security
    Status
