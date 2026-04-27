# E-Commerce Customer Churn Prediction
# Model Building & Evaluation

library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(pROC)
library(ROSE)
library(ggplot2)
library(dplyr)

df <- readRDS("data/ecommerce_churn_clean.rds")

# remove helper columns if present
df$Churn_label <- NULL

# train / test split 80/20 stratified
set.seed(42)
idx   <- createDataPartition(df$Churn, p = 0.8, list = FALSE)
train <- df[idx, ]
test  <- df[-idx, ]

# SMOTE to balance training set
train_bal <- ovun.sample(Churn ~ ., data = train,
                         method = "over", N = 8000, seed = 42)$data

cat("Training set class distribution after SMOTE:\n")
print(table(train_bal$Churn))

# ── Model 1: Logistic Regression ─────────────────────────────
log_model <- glm(Churn ~ ., data = train_bal, family = binomial(link = "logit"))
summary(log_model)

log_prob  <- predict(log_model, newdata = test, type = "response")
log_class <- ifelse(log_prob > 0.5, 1, 0)
log_class <- factor(log_class, levels = c(0, 1))

cat("\n=== Logistic Regression - Confusion Matrix ===\n")
print(confusionMatrix(log_class, test$Churn, positive = "1"))

# ── Model 2: Decision Tree ───────────────────────────────────
tree_model <- rpart(Churn ~ .,
                    data    = train_bal,
                    method  = "class",
                    control = rpart.control(maxdepth = 6, minsplit = 20))

png("charts/08_decision_tree.png", width = 1000, height = 700, res = 120)
rpart.plot(tree_model, type = 4, extra = 102,
           main = "Decision Tree - Customer Churn",
           cex  = 0.65)
dev.off()

tree_pred <- predict(tree_model, newdata = test, type = "class")

cat("\n=== Decision Tree - Confusion Matrix ===\n")
print(confusionMatrix(tree_pred, test$Churn, positive = "1"))

# ── Model 3: Random Forest ───────────────────────────────────
set.seed(42)
rf_model <- randomForest(Churn ~ .,
                         data       = train_bal,
                         ntree      = 500,
                         mtry       = 4,
                         importance = TRUE)

print(rf_model)

rf_pred <- predict(rf_model, newdata = test)

cat("\n=== Random Forest - Confusion Matrix ===\n")
print(confusionMatrix(rf_pred, test$Churn, positive = "1"))

# ── Feature Importance Plot ───────────────────────────────────
imp_df <- as.data.frame(importance(rf_model))
imp_df$Variable <- rownames(imp_df)
imp_df <- imp_df %>% arrange(desc(MeanDecreaseGini))

ggplot(imp_df[1:10, ], aes(x = reorder(Variable, MeanDecreaseGini),
                            y = MeanDecreaseGini, fill = MeanDecreaseGini)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "#13A8BF", high = "#0F2D5C") +
  labs(title = "Top 10 Predictors - Random Forest (MeanDecreaseGini)",
       x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("charts/09_feature_importance.png", width = 7, height = 5, dpi = 150)

# ── ROC Curves ───────────────────────────────────────────────
roc_log  <- roc(as.numeric(as.character(test$Churn)), log_prob)
roc_tree <- roc(as.numeric(as.character(test$Churn)),
                as.numeric(as.character(tree_pred)))
roc_rf   <- roc(as.numeric(as.character(test$Churn)),
                as.numeric(as.character(rf_pred)))

cat("\n=== AUC Scores ===\n")
cat("Logistic Regression AUC:", round(auc(roc_log), 3), "\n")
cat("Decision Tree AUC:      ", round(auc(roc_tree), 3), "\n")
cat("Random Forest AUC:      ", round(auc(roc_rf), 3), "\n")

png("charts/10_roc_curves.png", width = 700, height = 600, res = 120)
plot(roc_log,  col = "#13A8BF", lwd = 2,
     main = "ROC Curve Comparison")
lines(roc_tree, col = "#F5A623", lwd = 2)
lines(roc_rf,   col = "#0F2D5C", lwd = 2)
legend("bottomright",
       legend = c(paste("Logistic Regression AUC =", round(auc(roc_log),  3)),
                  paste("Decision Tree AUC =",       round(auc(roc_tree), 3)),
                  paste("Random Forest AUC =",       round(auc(roc_rf),   3))),
       col = c("#13A8BF", "#F5A623", "#0F2D5C"), lwd = 2)
dev.off()

# ── Save models ──────────────────────────────────────────────
saveRDS(log_model,  "outputs/logistic_model.rds")
saveRDS(tree_model, "outputs/decision_tree_model.rds")
saveRDS(rf_model,   "outputs/random_forest_model.rds")
