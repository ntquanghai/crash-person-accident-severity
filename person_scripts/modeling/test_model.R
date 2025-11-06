library(lightgbm)
library(dplyr)
library(caret)
library(tidyverse)
set.seed(123)

# 0. Prepare Data 
train_idx <- sample(seq_len(nrow(model_data)), 0.8 * nrow(model_data))
train_data <- model_data[train_idx, ]
test_data  <- model_data[-train_idx, ]

train_data$INJ_SEVERE_BINARY <- as.factor(train_data$INJ_SEVERE_BINARY)
test_data$INJ_SEVERE_BINARY  <- as.factor(test_data$INJ_SEVERE_BINARY)

train_matrix <- as.matrix(train_data %>% select(-INJ_SEVERE_BINARY))
train_label  <- as.numeric(as.character(train_data$INJ_SEVERE_BINARY))
test_matrix  <- as.matrix(test_data %>% select(-INJ_SEVERE_BINARY))
test_label   <- as.numeric(as.character(test_data$INJ_SEVERE_BINARY))

# ============================================================
# 1. Default LGBM (is_unbalance = TRUE)
# ============================================================
dtrain <- lgb.Dataset(data = train_matrix, label = train_label)
model_default <- lgb.train(
  params = list(objective = "binary", metric = "binary_logloss", is_unbalance = TRUE),
  data = dtrain, nrounds = 500, verbose = -1
)

# --- Train ---
train_pred_prob <- predict(model_default, train_matrix)
train_pred <- ifelse(train_pred_prob > 0.5, 1, 0)
cm_train1 <- confusionMatrix(factor(train_pred), factor(train_label), positive = "1")

# --- Test ---
pred_prob <- predict(model_default, test_matrix)
pred_bin <- ifelse(pred_prob > 0.5, 1, 0)
cm_test1 <- confusionMatrix(factor(pred_bin), factor(test_label), positive = "1")

cat("\n==============================\n[1] Default LGBM\n==============================\n")
cat("\nTrain Confusion Matrix:\n"); print(cm_train1$table)
cat("\nTest Confusion Matrix:\n"); print(cm_test1$table)
cat("\nTrain Metrics:\n")
cat(sprintf("Accuracy: %.3f | Precision: %.3f | Recall: %.3f | F1: %.3f\n",
            cm_train1$overall["Accuracy"], cm_train1$byClass["Precision"],
            cm_train1$byClass["Recall"], cm_train1$byClass["F1"]))
cat("Test Metrics:\n")
cat(sprintf("Accuracy: %.3f | Precision: %.3f | Recall: %.3f | F1: %.3f\n",
            cm_test1$overall["Accuracy"], cm_test1$byClass["Precision"],
            cm_test1$byClass["Recall"], cm_test1$byClass["F1"]))

# ============================================================
# 2. Undersampled LGBM
# ============================================================
maj <- train_data %>% filter(INJ_SEVERE_BINARY == 0)
min <- train_data %>% filter(INJ_SEVERE_BINARY == 1)
n <- min(nrow(maj), nrow(min))
under <- bind_rows(maj %>% sample_n(n), min %>% sample_n(n))
under$INJ_SEVERE_BINARY <- as.factor(under$INJ_SEVERE_BINARY)

under_mat <- as.matrix(under %>% select(-INJ_SEVERE_BINARY))
under_lbl <- as.numeric(as.character(under$INJ_SEVERE_BINARY))
dunder <- lgb.Dataset(data = under_mat, label = under_lbl)

model_under <- lgb.train(
  params = list(objective = "binary", metric = "binary_logloss"),
  data = dunder, nrounds = 500, verbose = -1
)

# --- Train ---
train_under_prob <- predict(model_under, under_mat)
train_under <- ifelse(train_under_prob > 0.5, 1, 0)
cm_train2 <- confusionMatrix(factor(train_under), factor(under_lbl), positive = "1")

# --- Test ---
pred_under_prob <- predict(model_under, test_matrix)
pred_under <- ifelse(pred_under_prob > 0.5, 1, 0)
cm_test2 <- confusionMatrix(factor(pred_under), factor(test_label), positive = "1")

cat("\n==============================\n[2] Undersampled LGBM\n==============================\n")
cat("\nTrain Confusion Matrix:\n"); print(cm_train2$table)
cat("\nTest Confusion Matrix:\n"); print(cm_test2$table)
cat("\nTrain Metrics:\n")
cat(sprintf("Accuracy: %.3f | Precision: %.3f | Recall: %.3f | F1: %.3f\n",
            cm_train2$overall["Accuracy"], cm_train2$byClass["Precision"],
            cm_train2$byClass["Recall"], cm_train2$byClass["F1"]))
cat("Test Metrics:\n")
cat(sprintf("Accuracy: %.3f | Precision: %.3f | Recall: %.3f | F1: %.3f\n",
            cm_test2$overall["Accuracy"], cm_test2$byClass["Precision"],
            cm_test2$byClass["Recall"], cm_test2$byClass["F1"]))

# ============================================================
# 3. Final Tuned LGBM
# ============================================================
results <- read_csv("lgbm_manualcv_undersampled.csv")
best_row <- results %>% slice(1)
final_params <- list(
  objective = "binary", boosting = "gbdt", metric = "binary_logloss",
  learning_rate = best_row$learning_rate, num_leaves = best_row$num_leaves,
  min_data_in_leaf = best_row$min_data_in_leaf, feature_fraction = best_row$feature_fraction,
  bagging_fraction = best_row$bagging_fraction, bagging_freq = best_row$bagging_freq,
  lambda_l2 = best_row$lambda_l2, verbose = -1
)

dunder <- lgb.Dataset(data = under_mat, label = under_lbl)
final_model <- lgb.train(params = final_params, data = dunder, nrounds = best_row$nrounds, verbose = -1)

# --- Train ---
train_final_prob <- predict(final_model, under_mat)
train_final <- ifelse(train_final_prob > 0.5, 1, 0)
cm_train3 <- confusionMatrix(factor(train_final), factor(under_lbl), positive = "1")

# --- Test ---
pred_final_prob <- predict(final_model, test_matrix)
pred_final <- ifelse(pred_final_prob > 0.5, 1, 0)
cm_test3 <- confusionMatrix(factor(pred_final), factor(test_label), positive = "1")

cat("\n==============================\n[3] Final Tuned LGBM\n==============================\n")
cat("\nTrain Confusion Matrix:\n"); print(cm_train3$table)
cat("\nTest Confusion Matrix:\n"); print(cm_test3$table)
cat("\nTrain Metrics:\n")
cat(sprintf("Accuracy: %.3f | Precision: %.3f | Recall: %.3f | F1: %.3f\n",
            cm_train3$overall["Accuracy"], cm_train3$byClass["Precision"],
            cm_train3$byClass["Recall"], cm_train3$byClass["F1"]))
cat("Test Metrics:\n")
cat(sprintf("Accuracy: %.3f | Precision: %.3f | Recall: %.3f | F1: %.3f\n",
            cm_test3$overall["Accuracy"], cm_test3$byClass["Precision"],
            cm_test3$byClass["Recall"], cm_test3$byClass["F1"]))
