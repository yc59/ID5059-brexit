brexit <- read.csv("master-copy.csv")

require(kknn)

head(brexit)
anyNA(brexit)
brexit$Remain <- as.numeric(brexit$Remain)

# remove parties with few members
data <- subset(brexit, !(Party == "Democratic Unionist Party"|Party == "Deputy Speaker"|Party == "Deputy Speaker"|
                           Party == "Green Party"|Party == "Plaid Cymru"|Party == "Sinn F?in"|Party == "Speaker"))
summary(data$Party)

# split dataset 
set.seed(12345) 
testIndex<- sample(1:623, 200, replace=F)
testData<- data[testIndex,]
trainData<- data[-testIndex,]

# make weighted k-Nearest Neighbor Classifier
weightedKnn <- kknn(Party ~ Remain + B + D + H + J + K + L + M + O, trainData,testData,
                    k = 7, distance = 2,kernel="triangular")
summary(weightedKnn)

# misclassification rate of test data
testData$predictParty <- fitted(weightedKnn)
View(testData)
test_misclassification <- 1 - sum(testData$predictParty == testData$Party)/nrow(testData) #0.045


