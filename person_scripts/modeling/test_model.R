# library(dplyr)
# library(ranger)
# library(caret)
# 
# results <- read.csv("rf_randomsearch_partial.csv")
# best_row <- results %>% arrange(desc(F1)) %>% slice(1)
# print(best_row)
# 
# # params
# num_trees <- best_row$num.trees
# mtry_val <- best_row$mtry
# min_node <- best_row$min.node.size
# samp_frac <- best_row$sample.fraction
# replace_opt <- as.logical(best_row$replace)
# max_depth <- ifelse(best_row$max.depth == 0, NULL, best_row$max.depth)
# 
# # training
# rf_tuned <- ranger(
#   INJ_SEVERE_BINARY ~ .,
#   data = train_under,
#   num.trees = num_trees,
#   mtry = mtry_val,
#   min.node.size = min_node,
#   sample.fraction = samp_frac,
#   replace = replace_opt,
#   max.depth = max_depth,
#   classification = TRUE,
#   probability = TRUE,
#   verbose = FALSE
# )
# 
# # eval-
# preds <- predict(rf_tuned, test_top40)$predictions[, 2]
# pred_bin <- ifelse(preds > 0.5, 1, 0)
# cm <- confusionMatrix(factor(pred_bin), factor(test_top40$INJ_SEVERE_BINARY), positive = "1")
# 
# print(cm$table)
# cat(sprintf("Accuracy: %.3f | Precision: %.3f | Recall: %.3f | F1: %.3f\n",
#             cm$overall["Accuracy"],
#             cm$byClass["Precision"],
#             cm$byClass["Recall"],
#             cm$byClass["F1"]))


# 4. Retrain Final Tuned Model
best_row <- results %>% slice(1)
final_params <- list(
  objective = "binary", boosting = "gbdt", metric = "binary_logloss",
  learning_rate = best_row$learning_rate, num_leaves = best_row$num_leaves,
  min_data_in_leaf = best_row$min_data_in_leaf, feature_fraction = best_row$feature_fraction,
  bagging_fraction = best_row$bagging_fraction, bagging_freq = best_row$bagging_freq,
  lambda_l2 = best_row$lambda_l2, verbose = -1
)

final_model <- lgb.train(params = final_params, data = dunder, nrounds = best_row$nrounds, verbose = -1)
pred_final_prob <- predict(final_model, test_matrix)
pred_final <- ifelse(pred_final_prob > 0.5, 1, 0)
cm3 <- confusionMatrix(factor(pred_final), factor(test_label), positive = "1")

cat("\n[4] Final Tuned LGBM Evaluation\n")
print(cm3$table)
cat(sprintf("Accuracy: %.3f | Precision: %.3f | Recall: %.3f | F1: %.3f\n",
            cm3$overall["Accuracy"], cm3$byClass["Precision"],
            cm3$byClass["Recall"], cm3$byClass["F1"]))