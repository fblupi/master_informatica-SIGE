# Set working directory and import datafiles
setwd("~/GitHub/master_informatica-SIGE/P1")
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Examine structure of dataframe
str(train)

# Look at number of people who survived
prop.table(table(train$Survived))

# Look at number of people who survived depending of the sex
summary(train$Sex)
prop.table(table(train$Sex))
prop.table(table(train$Sex, train$Survived), 1)

# Look people's age
summary(train$Age)

# Look at number of people who survived depending of the sex and the age
train$Child <- 0
train$Child[train$Age < 18] <- 1
prop.table(table(train$Child))
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# Look at class and fare patterns
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
aggregate(Survived ~ Child + Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# Create column child in test
test$Child <- 0
test$Child[test$Age < 18] <- 1

# Create new column in test set with our prediction that everyone dies
test$Survived <- 0
# Update the prediction to say that all females will survive
test$Survived[test$Sex == 'female'] <- 1

# Create submission dataframe and output to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "gendermodel.csv", row.names = FALSE)

# Create new column in test set with our prediction that everyone dies
test$Survived <- 0
# Update the prediction to say that all females will survive
test$Survived[test$Sex == 'female'] <- 1
# Update once more to say that females who pay more for a third class fare also perish
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0
# Update the prediction to say that all young males with 30+ fare and Pclass 1 or 2 survived
test$Survived[test$Sex == 'male' & test$Child == 1 & test$Fare >= 30 && (test$Pclass == 1 || test$Pclass == 2)] <- 1

# Create submission dataframe and output to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "genderclassmodel.csv", row.names = FALSE)