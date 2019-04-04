## Decision Tree

tree <- ctree(Party ~ B + D + H + J + K + L + M + O, data = train, 
              controls = ctree_control(mincriterion = 0.99, minsplit = 100))
                    ### Check the best mincriterion and minsplit??
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