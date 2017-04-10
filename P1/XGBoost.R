# Set working directory and import datafiles
setwd("~/GitHub/master_informatica-SIGE/P1")
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Install and load required packages for decision trees and forests
library(rpart)
install.packages('randomForest')
library(randomForest)
install.packages('party')
library(party)
install.packages('kernlab')
library(kernlab)
install.packages('caret')
library(caret)
install.packages('xgboost')
library(xgboost)

# Join together the test and train sets for easier feature engineering
test$Survived <- NA
combi <- rbind(train, test)

# Convert to a string
combi$Name <- as.character(combi$Name)

# Engineered variable: Title
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
# Combine small title groups
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# Convert to a factor
combi$Title <- factor(combi$Title)

# Engineered variable: Family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Engineered variable: Family
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
# Delete erroneous family IDs
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
# Convert to a factor
combi$FamilyID <- factor(combi$FamilyID)

# Fill in Age NAs
summary(combi$Age)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, 
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
# Check what else might be missing
summary(combi)
# Fill in Embarked blanks
summary(combi$Embarked)
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
# Fill in Fare NAs
summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

answer = as.numeric(combi$Survived[1:891])
combi = combi[,!(names(combi)%in%c('Survived','Name','Surname','Ticket','Cabin'))]

num_var_names<-sapply(combi,is.numeric)
num_var_names<-attr(num_var_names[num_var_names==T],"names")

combinum = combi[,(names(combi) %in% num_var_names)]
combifac = combi[,!(names(combi) %in% num_var_names)]

combifacDummy <- dummyVars("~.",data=combifac,fullRank=T)
combifac <- as.data.frame(predict(combifacDummy,combifac))

combi = cbind(combinum,combifac)

# Split back into test and train sets
train <- combi[1:891,]
train$Survived = answer
test <- combi[892:1309,]

predictorsNames = names(train)[names(train) != 'Survived'] #names of columns after data conversion
dtrain<-xgb.DMatrix(data=data.matrix(train[,predictorsNames]),label=train$Survived)
dtest<-data.matrix(test[,predictorsNames])

param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "logloss"
)

nround  = 3382
set.seed(2045)
clf <- xgboost(param =param, dtrain, nrounds=nround, min_child_weight=1,verbose=0)

pred <- predict(clf, dtest)
pred_final <- ifelse(pred>0.5,1,0)

submit <- data.frame(PassengerId = test$PassengerId, Survived = pred_final)
write.csv(submit, file = "subs_xgb_1.csv", row.names = FALSE) 