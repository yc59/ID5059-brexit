brexit <- read.csv("master-copy.csv")

require(nnet)

head(brexit)
anyNA(brexit)
brexit$Remain <- as.numeric(brexit$Remain)

# remove parties with few members
data <- subset(brexit, !(Party == "Democratic Unionist Party"|Party == "Deputy Speaker"|Party == "Deputy Speaker"|
                         Party == "Green Party"|Party == "Plaid Cymru"|Party == "Sinn F?in"|Party == "Speaker"))
summary(data$Party)

# split dataset 
set.seed(12345) 
testIndex<- sample(1:nrow(data), 200, replace=F)
testData<- data[testIndex,]
trainData<- data[-testIndex,]

# make multinomial logistic regression modle
multiLogit <- multinom(Party ~ Remain + B + D + H + J + K + L + M + O, data = trainData)
summary(multiLogit)

# calculate p-values using Wald tests (here z-tests)
z <- summary(multiLogit)$coefficients/summary(multiLogit)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# misclassification rate of train data
fitP <- fitted(multiLogit)
trainData$predictParty <- colnames(fitP)[max.col(fitP,ties.method="first")]
View(trainData)
train_misclassification <- 1 - sum(trainData$predictParty == trainData$Party)/nrow(trainData) #0.0212766

# misclassification rate of test data
predP <- predict(multiLogit, newdata = testData, "probs")
testData$predictParty <- colnames(predP)[max.col(predP,ties.method="first")]
View(testData)
test_misclassification <- 1 - sum(testData$predictParty == testData$Party)/nrow(testData) #0.07




