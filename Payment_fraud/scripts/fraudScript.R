# ML classifiers
library(readr)
library(dplyr)
library(caret)
library(doParallel)

# ML performance measures
perf_measures <- function(caret.fit, test_data, test_labels)
{
  
  df.fit <- cbind(obs = test_labels, pred = predict(caret.fit, newdata = test_data), 
                  predict(caret.fit, newdata = test_data, type = "prob"))
  df.cm <- confusionMatrix(data = df.fit$pred, reference = df.fit$obs, mode = 'everything')
  df.tcs <- twoClassSummary(df.fit, lev = levels(df.fit$obs))
  df.prs <- prSummary(df.fit, lev = levels(df.fit$obs))
  
  df.res <- data.frame(Model = caret.fit$modelInfo$label,
                       Accuracy = round(df.cm$overall[1],4),
                       ROC_AUC = round(df.tcs[1],4),
                       PR_AUC = round(df.prs[1],4))
}

#
original_data <- read_csv("~/Documents/Turing/DiffPrivacy/data.csv")
synthetic_data <- read_csv("~/Documents/Turing/DiffPrivacy/mst_data.csv")[,-1]
synthetic_data <- read_csv("~/Documents/Turing/DiffPrivacy/fraud_wl_data.csv")[,-1]

original_data <- original_data %>% mutate(across(where(is.numeric), as.factor))
synthetic_data <- synthetic_data %>% mutate(across(where(is.numeric), as.factor))

# Dummy vars for factors
dummies <- dummyVars(isFraud ~ ., data = original_data)
dummyData <- as.data.frame(predict(dummies, newdata = original_data))
comboInfo <- findLinearCombos(dummyData)
dummyData <- dummyData[, -c(comboInfo$remove, 5)]
rm(dummies)
rm(comboInfo)

# Caret Modelling
original_data_caret <- cbind(isFraud = original_data$isFraud, dummyData)
rm(dummyData)
levels(original_data_caret$isFraud) <- c('F0', 'F1')
original_data_caret$isFraud <- relevel(original_data_caret$isFraud, ref = 'F1')
str(original_data_caret)

# Dummy vars for factors
dummies <- dummyVars(isFraud ~ ., data = synthetic_data)
dummyData <- as.data.frame(predict(dummies, newdata = synthetic_data))
comboInfo <- findLinearCombos(dummyData)
dummyData <- dummyData[, -c(comboInfo$remove, 5)]
rm(dummies)
rm(comboInfo)

# Caret Modelling
synthetic_data_caret <- cbind(isFraud = synthetic_data$isFraud, dummyData)
rm(dummyData)
levels(synthetic_data_caret$isFraud) <- c('F0', 'F1')
synthetic_data_caret$isFraud <- relevel(synthetic_data_caret$isFraud, ref = 'F1')
str(synthetic_data_caret)

# Caret Modelling
original_train_data <- original_data_caret[1:50000,]
original_test_data <- original_data_caret[-c(1:50000),]
synthetic_train_data <- synthetic_data_caret[1:50000,]
synthetic_test_data <- synthetic_data_caret[-c(1:50000),]

set.seed(123)
cl <- makePSOCKcluster(15)
registerDoParallel(cl)

# prepare training scheme
control <- trainControl(method="none", classProbs = TRUE)
# GLM
fit.glm <- caret::train(isFraud~., data=original_train_data, method="bayesglm", trControl=control)
# TBAG
fit.tbag <- caret::train(isFraud~., data=original_train_data, method="treebag", trControl=control)

stopCluster(cl)

original_glmImp <- varImp(fit.glm, scale = FALSE)

# Predictions
results <- do.call(rbind, lapply(list(fit.glm, fit.tbag), perf_measures, original_test_data, original_test_data$isFraud))
row.names(results) <- NULL

print('Original Data Results')
results

set.seed(123)
cl <- makePSOCKcluster(15)
registerDoParallel(cl)

# prepare training scheme
control <- trainControl(method="none", classProbs = TRUE)
# GLM
fit.glm <- caret::train(isFraud~., data=synthetic_train_data, method="bayesglm", trControl=control)
# TBAG
fit.tbag <- caret::train(isFraud~., data=synthetic_train_data, method="treebag", trControl=control)

stopCluster(cl)

synthetic_glmImp <- varImp(fit.glm, scale = FALSE)

# Predictions
synthetic_results <- do.call(rbind, lapply(list(fit.glm, fit.tbag), perf_measures, original_test_data, original_test_data$isFraud))
row.names(synthetic_results) <- NULL

print('Sythetic Data Results')
synthetic_results

cbind(original_glmImp$importance, synthetic_glmImp$importance)

library(gridExtra)
pdf("figures/original_results.pdf", height=2.75, width=9.5)
grid.table(results)
dev.off()
pdf("figures/mst_results.pdf", height=2.75, width=9.5)
grid.table(synthetic_results)
dev.off()
pdf("figures/fraud_wl_results.pdf", height=1.2, width=5.8)
grid.table(synthetic_results)
dev.off()
