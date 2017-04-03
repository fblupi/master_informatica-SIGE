# Leer datos
adult.training.full <- read.csv("adult.training.csv", header = TRUE, sep = ",")
adult.test <- read.csv("adult.test.csv", header = TRUE, sep = ",")

# Eliminar filas con valores perdidos
adult.training <- adult.training.full[apply(adult.training.full, 1, function(row) !any (row %in% c("?"))), ]

# -- Cambiar nombres de valores e clasificación (caracteres >, <= son problemáticos)
adult.training$Class <- as.character(adult.training$Class)
adult.training$Class[adult.training$Class == ">50K"] <- "More50K"
adult.training$Class[adult.training$Class == "<=50K"] <- "Less50K"
adult.training$Class <- as.factor(adult.training$Class)

adult.test$Class <- as.character(adult.test$Class)
adult.test$Class[adult.test$Class == ">50K"] <- "More50K"
adult.test$Class[adult.test$Class == "<=50K"] <- "Less50K"
adult.test$Class <- as.factor(adult.test$Class)

# Bagging vs Boosting
training <- head(adult.training, n = 500)

# -- Bagging con Random Forest
set.seed(1)
library(caret)
rfCtrl  <- trainControl(method = "cv", number = 3, summaryFunction = twoClassSummary, classProbs = TRUE)
rfModel <- train(Class ~ . , data = training, method = "rf", allowParallel = TRUE, metric = "ROC", trControl = rfCtrl)
plot(rfModel)
plot(rfModel$finalModel)
rfPrediction<- predict(rfModel, adult.test, type = "prob")  # type = {raw, prob}
#confusionMatrix(rfPrediction, adult.test$Class)

# -- Boosting con xgboost
library(xgboost)
library(data.table)
library(Matrix)
df <- data.table(training, keep.rownames = FALSE)
sparse_matrix <- sparse.model.matrix(Class~.-1, data = df)
label_vector = df[,Class] == "Less50K"
bstModel <- xgboost(data = sparse_matrix, 
                    label = label_vector, 
                    max.depth = 6, 
                    eta = 1, 
                    nround = 5,
                    nthread = 2, 
                    objective = "binary:logistic")

importance <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bstModel)
xgb.plot.importance(importance_matrix = importance)
importanceRaw <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bstModel, data = sparse_matrix, label = label_vector)
importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]
head(importanceClean)

sparse_matrix_test <- sparse.model.matrix(Class~.-1, data = adult.test)
bstPrediction <- predict(bstModel, sparse_matrix_test)
bstPrediction_df <- data.frame(adult.test, bstPrediction)

# -- Comparación
comparison_df <- cbind(bstPrediction_df, rfPrediction)
colnames(comparison_df)[16] <- "Less50K_xgboost"
colnames(comparison_df)[17] <- "Less50K_rf"
colnames(comparison_df)[18] <- "More50K_rf"

# --> Completar analizando qué clasificador está funcionando mejor
