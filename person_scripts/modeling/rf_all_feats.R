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




# 1. Default Random Forest
rf_default <- ranger(
  INJ_SEVERE_BINARY ~ ., 
  data = train_data,
  num.trees = 500,
  mtry = floor(sqrt(ncol(train_data) - 1)),
  importance = "impurity",
  classification = TRUE,
  probability = TRUE,
  verbose = FALSE,
  class.weights = class_weights
)

pred_prob <- predict(rf_default, test_data)$predictions[, 2]
pred_bin <- ifelse(pred_prob > 0.5, 1, 0)
cm1 <- confusionMatrix(factor(pred_bin), factor(test_data$INJ_SEVERE_BINARY), positive = "1")

cat("\n[1] Default Random Forest\n")
print(cm1$table)
cat(sprintf("Accuracy: %.3f | Precision: %.3f | Recall: %.3f | F1: %.3f\n",
            cm1$overall["Accuracy"], cm1$byClass["Precision"],
            cm1$byClass["Recall"], cm1$byClass["F1"]))

# 2. Undersampled Random Forest
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

pred_prob_under <- predict(rf_under, test_data)$predictions[, 2]
pred_bin_under <- ifelse(pred_prob_under > 0.5, 1, 0)
cm2 <- confusionMatrix(factor(pred_bin_under), factor(test_data$INJ_SEVERE_BINARY), positive = "1")

cat("\n[2] Undersampled Random Forest\n")
print(cm2$table)
cat(sprintf("Accuracy: %.3f | Precision: %.3f | Recall: %.3f | F1: %.3f\n",
            cm2$overall["Accuracy"], cm2$byClass["Precision"],
            cm2$byClass["Recall"], cm2$byClass["F1"]))

# 3. Hyperparameter Tuning (Random Search + 5-Fold CV)
f1_score <- function(pred, true) {
  cm <- table(Pred = pred, Actual = true)
  TP <- ifelse("1" %in% rownames(cm) && "1" %in% colnames(cm), cm["1", "1"], 0)
  FP <- ifelse("1" %in% rownames(cm) && "0" %in% colnames(cm), cm["1", "0"], 0)
  FN <- ifelse("0" %in% rownames(cm) && "1" %in% colnames(cm), cm["0", "1"], 0)
  prec <- ifelse(TP + FP == 0, 0, TP / (TP + FP))
  rec  <- ifelse(TP + FN == 0, 0, TP / (TP + FN))
  if ((prec + rec) == 0) return(0)
  2 * prec * rec / (prec + rec)
}

tune_grid <- data.frame(
  num.trees = sample(seq(200, 800, 100), 30, replace = TRUE),
  mtry = sample(2:(ncol(under) - 1), 30, replace = TRUE),
  min.node.size = sample(1:20, 30, replace = TRUE),
  sample.fraction = runif(30, 0.6, 1.0),
  replace = sample(c(TRUE, FALSE), 30, replace = TRUE),
  max.depth = sample(c(0, 5, 10, 15, 20), 30, replace = TRUE)
)

folds <- createFolds(under$INJ_SEVERE_BINARY, k = 5)
results <- data.frame(); best_score <- -Inf; best_params <- NULL
cat("\n[3] Manual RF 5-Fold Random Search...\n")

for (i in 1:nrow(tune_grid)) {
  p <- tune_grid[i, ]; f1s <- c()
  for (fold in 1:5) {
    val_idx <- folds[[fold]]
    train_fold <- under[-val_idx, ]; val_fold <- under[val_idx, ]
    
    rf <- ranger(
      INJ_SEVERE_BINARY ~ ., 
      data = train_fold,
      num.trees = p$num.trees,
      mtry = p$mtry,
      min.node.size = p$min.node.size,
      sample.fraction = p$sample.fraction,
      replace = p$replace,
      max.depth = if (p$max.depth == 0) NULL else p$max.depth,
      classification = TRUE,
      probability = TRUE,
      verbose = FALSE
    )
    
    preds <- predict(rf, val_fold)$predictions[, 2]
    preds_bin <- ifelse(preds > 0.5, 1, 0)
    f1s <- c(f1s, f1_score(preds_bin, val_fold$INJ_SEVERE_BINARY))
  }
  f1_mean <- mean(f1s)
  results <- rbind(results, cbind(tune_grid[i, ], F1 = f1_mean))
  if (f1_mean > best_score) { best_score <- f1_mean; best_params <- p }
  cat(sprintf("Combo %02d → F1=%.4f | Best=%.4f\n", i, f1_mean, best_score))
}

results <- results %>% arrange(desc(F1))
write.csv(results, "rf_manualcv_undersampled.csv", row.names = FALSE)

cat("\n[3] Best Tuned RF\n")
cat(sprintf("Best F1: %.4f\n", best_score))
print(best_params)

# 4. Retrain Final Tuned Model
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

pred_final_prob <- predict(final_rf, test_data)$predictions[, 2]
pred_final <- ifelse(pred_final_prob > 0.5, 1, 0)
cm3 <- confusionMatrix(factor(pred_final), factor(test_data$INJ_SEVERE_BINARY), positive = "1")

cat("\n[4] Final Tuned RF Evaluation\n")
print(cm3$table)
cat(sprintf("Accuracy: %.3f | Precision: %.3f | Recall: %.3f | F1: %.3f\n",
            cm3$overall["Accuracy"], cm3$byClass["Precision"],
            cm3$byClass["Recall"], cm3$byClass["F1"]))
