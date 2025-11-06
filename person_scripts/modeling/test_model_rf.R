library(ranger)
library(dplyr)
library(caret)
set.seed(123)

# 0. Prepare Data 
train_idx <- sample(seq_len(nrow(model_data)), 0.8 * nrow(model_data))
train_data <- model_data[train_idx, ]
test_data  <- model_data[-train_idx, ]

train_data$INJ_SEVERE_BINARY <- as.factor(train_data$INJ_SEVERE_BINARY)
test_data$INJ_SEVERE_BINARY  <- as.factor(test_data$INJ_SEVERE_BINARY)

class_counts <- table(train_data$INJ_SEVERE_BINARY)
ratio <- unname(class_counts["0"] / class_counts["1"])
class_weights <- c("0" = 1, "1" = ratio)

# ============================================================
# 1. Weighted Random Forest
# ============================================================
rf_default <- ranger(
  INJ_SEVERE_BINARY ~ ., 
  data = train_data,
  num.trees = 500,
  mtry = floor(sqrt(ncol(train_data) - 1)),
  classification = TRUE,
  probability = TRUE,
  verbose = FALSE,
  class.weights = class_weights
)

# --- Train ---
train_prob <- predict(rf_default, train_data)$predictions[, 2]
train_bin <- ifelse(train_prob > 0.5, 1, 0)
cm_train1 <- confusionMatrix(factor(train_bin), factor(train_data$INJ_SEVERE_BINARY), positive = "1")

# --- Test ---
test_prob <- predict(rf_default, test_data)$predictions[, 2]
test_bin <- ifelse(test_prob > 0.5, 1, 0)
cm_test1 <- confusionMatrix(factor(test_bin), factor(test_data$INJ_SEVERE_BINARY), positive = "1")

cat("\n==============================\n[1] Weighted Random Forest\n==============================\n")
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
# 2. Undersampled Random Forest
# ============================================================
maj <- train_data %>% filter(INJ_SEVERE_BINARY == 0)
min <- train_data %>% filter(INJ_SEVERE_BINARY == 1)
n <- min(nrow(maj), nrow(min))
under <- bind_rows(maj %>% sample_n(n), min %>% sample_n(n))

rf_under <- ranger(
  INJ_SEVERE_BINARY ~ ., 
  data = under,
  num.trees = 500,
  mtry = floor(sqrt(ncol(under) - 1)),
  classification = TRUE,
  probability = TRUE,
  verbose = FALSE
)

# --- Train ---
train_under_prob <- predict(rf_under, under)$predictions[, 2]
train_under_bin <- ifelse(train_under_prob > 0.5, 1, 0)
cm_train2 <- confusionMatrix(factor(train_under_bin), factor(under$INJ_SEVERE_BINARY), positive = "1")

# --- Test ---
pred_prob_under <- predict(rf_under, test_data)$predictions[, 2]
pred_bin_under <- ifelse(pred_prob_under > 0.5, 1, 0)
cm_test2 <- confusionMatrix(factor(pred_bin_under), factor(test_data$INJ_SEVERE_BINARY), positive = "1")

cat("\n==============================\n[2] Undersampled Random Forest\n==============================\n")
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
# 3. Final Tuned Random Forest
# ============================================================
results <- read.csv("rf_manualcv_undersampled.csv")
best_row <- results %>% slice(1)

final_rf <- ranger(
  INJ_SEVERE_BINARY ~ ., 
  data = under,
  num.trees = best_row$num.trees,
  mtry = best_row$mtry,
  min.node.size = best_row$min.node.size,
  sample.fraction = best_row$sample.fraction,
  replace = best_row$replace,
  max.depth = if (best_row$max.depth == 0) NULL else best_row$max.depth,
  classification = TRUE,
  probability = TRUE,
  verbose = FALSE
)

# --- Train ---
train_final_prob <- predict(final_rf, under)$predictions[, 2]
train_final_bin <- ifelse(train_final_prob > 0.5, 1, 0)
cm_train3 <- confusionMatrix(factor(train_final_bin), factor(under$INJ_SEVERE_BINARY), positive = "1")

# --- Test ---
pred_final_prob <- predict(final_rf, test_data)$predictions[, 2]
pred_final_bin <- ifelse(pred_final_prob > 0.5, 1, 0)
cm_test3 <- confusionMatrix(factor(pred_final_bin), factor(test_data$INJ_SEVERE_BINARY), positive = "1")

cat("\n==============================\n[3] Final Tuned Random Forest\n==============================\n")
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
