library(lightgbm)
library(dplyr)
library(caret)
library(SHAPforxgboost)
library(ggplot2)

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

# 2. Undersampled LGBM
maj <- train_data %>% filter(INJ_SEVERE_BINARY == 0)
min <- train_data %>% filter(INJ_SEVERE_BINARY == 1)
n <- min(nrow(maj), nrow(min))
under <- bind_rows(maj %>% sample_n(n), min %>% sample_n(n))
under$INJ_SEVERE_BINARY <- as.factor(under$INJ_SEVERE_BINARY)

under_mat <- as.matrix(under %>% select(-INJ_SEVERE_BINARY))
under_lbl <- as.numeric(as.character(under$INJ_SEVERE_BINARY))
dunder <- lgb.Dataset(data = under_mat, label = under_lbl)

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
cat("\n[5] Computing SHAP values...\n")

# Get SHAP values
shap_values <- predict(final_model, test_matrix, type = "contrib")
shap_df <- as.data.frame(shap_values)[, -ncol(shap_values)]
# colnames(shap_df) <- top40_vars
feature_names <- colnames(train_matrix)
colnames(shap_df) <- feature_names

# Mean absolute SHAP importance
mean_abs_shap <- data.frame(
  Feature = colnames(shap_df),
  MeanAbsSHAP = apply(abs(shap_df), 2, mean)
) %>%
  arrange(desc(MeanAbsSHAP))

cat("\nTop 10 most influential features (Mean |SHAP|):\n")
print(head(mean_abs_shap, 10))


cat("\n[5] Computing SHAP values...\n")

# Get SHAP values (each column = feature, last column = bias)
shap_values <- predict(final_model, test_matrix, type = "contrib")
shap_df <- as.data.frame(shap_values[, -ncol(shap_values), drop = FALSE])
feature_names <- colnames(train_matrix)
colnames(shap_df) <- feature_names

# Mean absolute SHAP importance
mean_abs_shap <- data.frame(
  Feature = feature_names,
  MeanAbsSHAP = apply(abs(shap_df), 2, mean)
) %>%
  arrange(desc(MeanAbsSHAP))

cat("\nTop 10 most influential features (Mean |SHAP|):\n")
print(head(mean_abs_shap, 10))

# Global bar plot
ggplot(mean_abs_shap[1:20, ], aes(x = reorder(Feature, MeanAbsSHAP), y = MeanAbsSHAP)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 SHAP Feature Importances", x = "Feature", y = "Mean |SHAP| Value") +
  theme_minimal(base_size = 13)

# Beeswarm plot
shap_score <- shap_df
X <- as.data.frame(test_matrix)
colnames(X) <- feature_names

png("shap_beeswarm.png", width = 900, height = 700)
shap.plot.summary.wrap2(shap_score = shap_score, X = X, top_n = 20, dilute = TRUE)
dev.off()
cat("\n[6] SHAP beeswarm plot saved as shap_beeswarm.png\n")

