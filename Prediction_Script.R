#https://rpubs.com/renrele/loanpred

library(ggplot2)
library(mlr)

train <-read.csv("~/Documents/College/Second Year/Semester 2/DS/CP - Loan Prediction System/Data/train.csv",na.strings = c(""," ",NA))
test <-read.csv("~/Documents/College/Second Year/Semester 2/DS/CP - Loan Prediction System/Data/test.csv",na.strings = c(""," ",NA))

print(summarizeColumns(train))
print(summarizeColumns(test))

print(barplot(table(train$Loan_Status)))
print(prop.table(table(train$Loan_Status)))

par(mfrow=c(1,2))
print(barplot(table(train$Gender),main="train set"))
print(barplot(table(test$Gender),main="test set"))

print(prop.table(table(train$Gender)))
print(prop.table(table(test$Gender)))

par(mfrow=c(1,2))
print(barplot(table(train$Married),main="train set"))
print(barplot(table(test$Married),main="test set"))

print(prop.table(table(train$Married)))
print(prop.table(table(test$Married)))

par(mfrow=c(1,2))
print(levels(train$Dependents))

print(barplot(table(train$Dependents),main="train set"))
print(barplot(table(test$Dependents),main="test set"))

print(prop.table(table(train$Dependents)))
print(prop.table(table(test$Dependents)))

