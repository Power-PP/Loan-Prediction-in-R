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

par(mfrow=c(1,2))
levels(train$Education)
barplot(table(train$Education),main="train set")
barplot(table(test$Education),main="test set")
prop.table(table(train$Education))
prop.table(table(test$Education))

par(mfrow=c(1,2))
levels(train$Self_Employed)
barplot(table(train$Self_Employed),main="train set")
barplot(table(test$Self_Employed),main="test set")
prop.table(table(train$Self_Employed))
prop.table(table(test$Self_Employed))

ar(mfrow=c(1,2))
boxplot(train$ApplicantIncome,train$CoapplicantIncome,names=c("App Income","Coapp Income"),main="train set")
boxplot(test$ApplicantIncome,test$CoapplicantIncome,names=c("App Income","Coapp Income"),main="test set")


par(mfrow=c(1,2))
boxplot(train$LoanAmount,main="train set")
boxplot(test$LoanAmount,main="test set")

par(mfrow=c(1,2))
hist(train$Loan_Amount_Term,breaks=500,main="train set")
hist(test$Loan_Amount_Term,breaks=500,main="test set")
summary(train$Loan_Amount_Term)
summary(test$Loan_Amount_Term)

par(mfrow=c(1,2))
train$Credit_History <-as.factor(train$Credit_History)
test$Credit_History <- as.factor(test$Credit_History)
barplot(table(train$Credit_History),main="train set")
barplot(table(test$Credit_History),main="test set")
prop.table(table(train$Credit_History))
prop.table(table(test$Credit_History)) 

par(mfrow=c(1,2))
barplot(table(train$Property_Area),main="train set")
barplot(table(test$Property_Area),main="test set")
prop.table(table(train$Property_Area))
prop.table(table(test$Property_Area))

# Other Analysis

print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Gender)+ggtitle("Loan Status by Gender of Applicant"))


print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Married)+ggtitle("Loan Status by Marital Status of Applicant"))


#a larger proportion of not married applicants are refused than mmaried ones
print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Dependents)+ggtitle("Loan Status by number of Dependents of Applicant"))


#a smaller proportion of applicants with 2 dependents is refused than other numbers
print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Education)+ggtitle("Loan Status by Education of Applicant"))


#a larger proportion on non graduates are refused than graduates
print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Self_Employed)+ggtitle("Loan Status by Employment status of Applicant"))


#not self employed seems to be slightly preferred
print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Loan_Amount_Term)+ggtitle("Loan Status by terms  of loan"))


#difficult to see any patterns, most of the loans are for 360 months
print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Credit_History)+ggtitle("Loan Status by credit history of Applicant"))


#this looks very important! Almost all applicants with history=0 are refused
print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Property_Area)+ggtitle("Loan Status by property area"))


#it's easiest to get a loan if the property is semi urban and hardest if it is rural
print(ggplot(train, aes(x=Loan_Status,y=ApplicantIncome))+geom_boxplot()+ggtitle("Loan Status by Applicant income"))


#doesn't look like there's much difference
print(ggplot(train, aes(x=Loan_Status,y=CoapplicantIncome))+geom_boxplot()+ggtitle("Loan Status by coapplicant income"))


#this seems to make a difference
print(ggplot(train, aes(x=Loan_Status,y=LoanAmount))+geom_boxplot()+ggtitle("Loan Status by Loan Amount"))

