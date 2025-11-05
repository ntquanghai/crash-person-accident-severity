library(lightgbm)
library(dplyr)
library(caret)
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

# 1. Default LGBM (is_unbalance = TRUE)
dtrain <- lgb.Dataset(data = train_matrix, label = train_label)
model_default <- lgb.train(
  params = list(objective = "binary", metric = "binary_logloss", is_unbalance = TRUE),
  data = dtrain, nrounds = 500, verbose = -1
)
pred_prob <- predict(model_default, test_matrix)
pred_bin <- ifelse(pred_prob > 0.5, 1, 0)
cm1 <- confusionMatrix(factor(pred_bin), factor(test_label), positive = "1")
cat("\n[1] Default LGBM (is_unbalance = TRUE)\n")
print(cm1$table)
cat(sprintf("Accuracy: %.3f | Precision: %.3f | Recall: %.3f | F1: %.3f\n",
            as.numeric(cm1$overall["Accuracy"]),
            as.numeric(cm1$byClass["Precision"]),
            as.numeric(cm1$byClass["Recall"]),
            as.numeric(cm1$byClass["F1"])))

# 2. Undersampled LGBM
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
pred_under_prob <- predict(model_under, test_matrix)
pred_under <- ifelse(pred_under_prob > 0.5, 1, 0)
cm2 <- confusionMatrix(factor(pred_under), factor(test_label), positive = "1")
cat("\n[2] Undersampled LGBM\n")
print(cm2$table)
cat(sprintf("Accuracy: %.3f | Precision: %.3f | Recall: %.3f | F1: %.3f\n",
            as.numeric(cm2$overall["Accuracy"]),
            as.numeric(cm2$byClass["Precision"]),
            as.numeric(cm2$byClass["Recall"]),
            as.numeric(cm2$byClass["F1"])))

# 3. Hyperparameter Tuning 
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
  learning_rate = runif(30, 0.01, 0.3),
  nrounds = sample(seq(200, 800, 50), 30, replace = TRUE),
  num_leaves = sample(15:255, 30, replace = TRUE),
  min_data_in_leaf = sample(10:100, 30, replace = TRUE),
  feature_fraction = runif(30, 0.6, 1.0),
  bagging_fraction = runif(30, 0.6, 1.0),
  bagging_freq = sample(1:10, 30, replace = TRUE),
  lambda_l2 = runif(30, 0, 1)
)

folds <- createFolds(under$INJ_SEVERE_BINARY, k = 5)
results <- data.frame(); best_score <- -Inf; best_params <- NULL
cat("\n[3] Manual LGBM 5-Fold Random Search...\n")

for (i in 1:nrow(tune_grid)) {
  p <- tune_grid[i, ]; f1_scores <- c()
  
  for (fold in 1:5) {
    val_idx <- folds[[fold]]
    train_fold <- under[-val_idx, ]
    val_fold   <- under[val_idx, ]
    
    train_mat <- as.matrix(train_fold %>% select(-INJ_SEVERE_BINARY))
    val_mat   <- as.matrix(val_fold %>% select(-INJ_SEVERE_BINARY))
    train_lbl <- as.numeric(as.character(train_fold$INJ_SEVERE_BINARY))
    val_lbl   <- as.numeric(as.character(val_fold$INJ_SEVERE_BINARY))
    
    dtrain <- lgb.Dataset(data = train_mat, label = train_lbl, free_raw_data = FALSE)
    params <- list(
      objective = "binary", boosting = "gbdt", metric = "binary_logloss",
      learning_rate = p$learning_rate, num_leaves = p$num_leaves,
      min_data_in_leaf = p$min_data_in_leaf, feature_fraction = p$feature_fraction,
      bagging_fraction = p$bagging_fraction, bagging_freq = p$bagging_freq,
      lambda_l2 = p$lambda_l2, feature_pre_filter = FALSE, verbose = -1
    )
    
    model <- lgb.train(params = params, data = dtrain, nrounds = p$nrounds, verbose = -1)
    preds <- predict(model, val_mat)
    preds_bin <- ifelse(preds > 0.5, 1, 0)
    f1_scores <- c(f1_scores, f1_score(preds_bin, val_lbl))
  }
  
  f1_mean <- mean(f1_scores)
  results <- rbind(results, cbind(tune_grid[i, ], F1 = f1_mean))
  if (f1_mean > best_score) { best_score <- f1_mean; best_params <- p }
  cat(sprintf("Combo %02d → F1=%.4f | Best=%.4f\n", i, f1_mean, best_score))
}

results <- results %>% arrange(desc(F1))
write.csv(results, "lgbm_manualcv_undersampled_fullfeats.csv", row.names = FALSE)
cat("\n[3] Best Tuned LGBM\n")
cat(sprintf("Best F1: %.4f\n", best_score))
print(best_params)

# 4. Retrain Final Tuned Model
best_row <- results %>% slice(1)
final_params <- list(
  objective = "binary", boosting = "gbdt", metric = "binary_logloss",
  learning_rate = best_row$learning_rate, num_leaves = best_row$num_leaves,
  min_data_in_leaf = best_row$min_data_in_leaf, feature_fraction = best_row$feature_fraction,
  bagging_fraction = best_row$bagging_fraction, bagging_freq = best_row$bagging_freq,
  lambda_l2 = best_row$lambda_l2, verbose = -1
)

dunder <- lgb.Dataset(
  data = as.matrix(under %>% select(-INJ_SEVERE_BINARY)),
  label = as.numeric(as.character(under$INJ_SEVERE_BINARY))
)

final_model <- lgb.train(params = final_params, data = dunder, nrounds = best_row$nrounds, verbose = -1)
pred_final_prob <- predict(final_model, test_matrix)
pred_final <- ifelse(pred_final_prob > 0.5, 1, 0)
cm3 <- confusionMatrix(factor(pred_final), factor(test_label), positive = "1")

cat("\n[4] Final Tuned LGBM Evaluation\n")
print(cm3$table)
cat(sprintf("Accuracy: %.3f | Precision: %.3f | Recall: %.3f | F1: %.3f\n",
            as.numeric(cm3$overall["Accuracy"]),
            as.numeric(cm3$byClass["Precision"]),
            as.numeric(cm3$byClass["Recall"]),
            as.numeric(cm3$byClass["F1"])))
