library(car)
library(caret)
library(corrplot)
library(randomForest)
library(cowplot)
library(party)
library(gam)
library(nnet)

data <- read.csv("~/Desktop/ID5059/Practical02/ID5059-P2/master-copy.csv")
attach(data)
summary(data)
str(data)
table(data$Party)

# Data Partition
set.seed(1234)
pd <- sample(2, nrow(data), replace = TRUE, prob = c(0.8, 0.2))
train <- data[pd == 1, ]
validate <- data[pd == 2, ]

## Random Forest

rf <- randomForest(Party ~ B + D + H + J + K + L + M + O, data = train,
                   ntree = 300, mtry = 8, importance = TRUE, proximity = TRUE)
print(rf)
attributes(rf)

#Predict for train data
p1 <- predict(rf, data = train)
confusionMatrix(p1, train$Party)

#Predict for validate data
p2 <- predict(rf, data = validate)
confusionMatrix(p2, validate$Party)

plot(rf)

hist(treesize(rf), main = "No. of Nodes for the trees")
varImpPlot(rf, sort = TRUE, n.var = 8, main = "Top variable importance")
importance(rf)
varUsed(rf)

#Misclassification for train data
tab1 <- table(predict(rf), train$Party)
print(rf)
1-sum(diag(tab1))/sum(tab1)

#Misclassification for validate data
testPred1 <- predict(tree, newdata = validate)
tab2 <- table(testPred1, validate$Party)
print(tab2)
1-sum(diag(tab2))/sum(tab2)

## Decision Tree

tree <- ctree(Party ~ B + D + H + J + K + L + M + O, data = train, 
              controls = ctree_control(mincriterion = 0.99, minsplit = 100))
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

