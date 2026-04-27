# E-Commerce Customer Churn Prediction
# Exploratory Data Analysis

library(ggplot2)
library(dplyr)
library(corrplot)

df <- readRDS("data/ecommerce_churn_clean.rds")

df$Churn_label <- ifelse(as.numeric(as.character(df$Churn)) == 1, "Churned", "Retained")

# ── Figure 1: Churn Distribution ────────────────────────────
ggplot(df, aes(x = Churn_label, fill = Churn_label)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Churned" = "#F5A623", "Retained" = "#0D7C8F")) +
  labs(title = "Churn Distribution", x = "Churn Status", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("charts/01_churn_distribution.png", width = 6, height = 4, dpi = 150)

# ── Figure 2: Tenure vs Churn ────────────────────────────────
ggplot(df, aes(x = Churn_label, y = Tenure, fill = Churn_label)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("Churned" = "#F5A623", "Retained" = "#0D7C8F")) +
  labs(title = "Tenure by Churn Status", x = "Churn Status", y = "Tenure (Months)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("charts/02_tenure_vs_churn.png", width = 6, height = 4, dpi = 150)

# ── Figure 3: Satisfaction Score vs Churn ────────────────────
df %>%
  group_by(SatisfactionScore, Churn_label) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(SatisfactionScore) %>%
  mutate(pct = n / sum(n) * 100) %>%
  filter(Churn_label == "Churned") %>%
  ggplot(aes(x = factor(SatisfactionScore), y = pct, fill = factor(SatisfactionScore))) +
  geom_col() +
  geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.5, size = 3.5) +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Churn Rate by Satisfaction Score",
       x = "Satisfaction Score (1=Low, 5=High)", y = "Churn Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("charts/03_satisfaction_vs_churn.png", width = 6, height = 4, dpi = 150)

# ── Figure 4: Preferred Order Category vs Churn ──────────────
cat_labels <- c("1" = "Fashion", "2" = "Grocery", "3" = "Laptop",
                "4" = "Mobile Phone", "5" = "Others")

df %>%
  mutate(Cat = recode(as.character(PreferedOrderCat), !!!cat_labels)) %>%
  group_by(Cat, Churn_label) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Cat) %>%
  mutate(pct = n / sum(n) * 100) %>%
  filter(Churn_label == "Churned") %>%
  ggplot(aes(x = reorder(Cat, pct), y = pct, fill = pct)) +
  geom_col() +
  geom_text(aes(label = paste0(round(pct, 1), "%")), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_fill_gradient(low = "#0D7C8F", high = "#0F2D5C") +
  labs(title = "Churn Rate by Preferred Order Category",
       x = "Order Category", y = "Churn Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("charts/04_ordercategory_vs_churn.png", width = 7, height = 4, dpi = 150)

# ── Figure 5: Cashback Amount - Churned vs Retained ──────────
df %>%
  group_by(Churn_label) %>%
  summarise(avg_cashback = mean(CashbackAmount, na.rm = TRUE)) %>%
  ggplot(aes(x = Churn_label, y = avg_cashback, fill = Churn_label)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0("$", round(avg_cashback, 1))), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Churned" = "#F5A623", "Retained" = "#0D7C8F")) +
  labs(title = "Average Cashback: Churned vs Retained",
       x = "Churn Status", y = "Average Cashback ($)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("charts/05_cashback_vs_churn.png", width = 6, height = 4, dpi = 150)

# ── Figure 6: City Tier vs Churn ─────────────────────────────
df %>%
  group_by(CityTier, Churn_label) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(CityTier) %>%
  mutate(pct = n / sum(n) * 100) %>%
  filter(Churn_label == "Churned") %>%
  ggplot(aes(x = factor(CityTier), y = pct, fill = factor(CityTier))) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("1" = "#0F2D5C", "2" = "#0D7C8F", "3" = "#F5A623")) +
  labs(title = "Churn Rate by City Tier",
       x = "City Tier", y = "Churn Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("charts/06_citytier_vs_churn.png", width = 6, height = 4, dpi = 150)

# ── Figure 7: Correlation Heatmap ────────────────────────────
num_vars <- df %>%
  select(Tenure, SatisfactionScore, NumberOfDeviceRegistered,
         CashbackAmount, DaySinceLastOrder, WarehouseToHome,
         NumberOfAddress, CouponUsed, OrderCount, Complain) %>%
  mutate(Churn_num = as.numeric(as.character(df$Churn)))

cor_matrix <- cor(num_vars, use = "complete.obs")

png("charts/07_correlation_heatmap.png", width = 800, height = 700, res = 120)
corrplot(cor_matrix,
         method = "color",
         type   = "lower",
         tl.cex = 0.75,
         tl.col = "black",
         addCoef.col = "black",
         number.cex  = 0.6,
         col = colorRampPalette(c("#F5A623", "white", "#0D7C8F"))(200),
         title = "Correlation Matrix - Numeric Variables",
         mar = c(0, 0, 2, 0))
dev.off()
