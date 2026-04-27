# E-Commerce Customer Churn Prediction
# Model Results Summary

library(ggplot2)
library(dplyr)

# model performance summary table
results <- data.frame(
  Model     = c("Logistic Regression", "Decision Tree", "Random Forest"),
  Accuracy  = c(82.4, 88.1, 93.5),
  Precision = c(71.2, 78.6, 89.4),
  Recall    = c(68.5, 75.3, 88.1),
  F1_Score  = c(69.8, 76.9, 88.7),
  AUC_ROC   = c(0.843, 0.872, 0.961)
)

print(results)

# accuracy comparison bar chart
ggplot(results, aes(x = reorder(Model, Accuracy), y = Accuracy, fill = Model)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(Accuracy, "%")), hjust = -0.1, size = 4) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Logistic Regression" = "#13A8BF",
    "Decision Tree"       = "#0D7C8F",
    "Random Forest"       = "#0F2D5C"
  )) +
  ylim(0, 105) +
  labs(title = "Model Accuracy Comparison", x = "", y = "Accuracy (%)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("charts/11_accuracy_comparison.png", width = 7, height = 4, dpi = 150)

# AUC-ROC comparison
ggplot(results, aes(x = reorder(Model, AUC_ROC), y = AUC_ROC, fill = Model)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = AUC_ROC), hjust = -0.1, size = 4) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Logistic Regression" = "#13A8BF",
    "Decision Tree"       = "#0D7C8F",
    "Random Forest"       = "#0F2D5C"
  )) +
  ylim(0, 1.1) +
  labs(title = "AUC-ROC Comparison", x = "", y = "AUC-ROC Score") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("charts/12_aucroc_comparison.png", width = 7, height = 4, dpi = 150)

write.csv(results, "outputs/model_results_summary.csv", row.names = FALSE)
