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

## Random Forest

rf <- randomForest(Party ~ B + D + H + J + K + L + M + O, data = train,
                   ntree = 300, mtry = 8, importance = TRUE, proximity = TRUE)
                    #### Check the best ntree and mtry??
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