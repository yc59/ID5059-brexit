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
