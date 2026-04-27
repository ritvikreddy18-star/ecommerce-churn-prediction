# E-Commerce Customer Churn Prediction
# Data Cleaning & Feature Engineering

library(readxl)
library(dplyr)

df <- read_excel("data/ecommerce_churn.xlsx")

# fix missing values with median
df$Tenure[is.na(df$Tenure)] <- median(df$Tenure, na.rm = TRUE)
df$WarehouseToHome[is.na(df$WarehouseToHome)] <- median(df$WarehouseToHome, na.rm = TRUE)
df$HourSpendOnApp[is.na(df$HourSpendOnApp)] <- median(df$HourSpendOnApp, na.rm = TRUE)
df$OrderAmountHikeFromlastYear[is.na(df$OrderAmountHikeFromlastYear)] <- median(df$OrderAmountHikeFromlastYear, na.rm = TRUE)
df$CouponUsed[is.na(df$CouponUsed)] <- median(df$CouponUsed, na.rm = TRUE)
df$OrderCount[is.na(df$OrderCount)] <- median(df$OrderCount, na.rm = TRUE)
df$DaySinceLastOrder[is.na(df$DaySinceLastOrder)] <- median(df$DaySinceLastOrder, na.rm = TRUE)

# fix outliers in WarehouseToHome
df$WarehouseToHome[df$WarehouseToHome == 126] <- 26
df$WarehouseToHome[df$WarehouseToHome == 127] <- 27

# fix inconsistent label
df$PreferredLoginDevice <- gsub("Mobile Phone", "Phone", df$PreferredLoginDevice)

# encode target as factor
df$Churn <- as.factor(df$Churn)

# encode categorical variables
df$PreferredLoginDevice   <- as.numeric(as.factor(df$PreferredLoginDevice))
df$PreferredPaymentMode   <- as.numeric(as.factor(df$PreferredPaymentMode))
df$Gender                 <- as.numeric(as.factor(df$Gender))
df$PreferedOrderCat       <- as.numeric(as.factor(df$PreferedOrderCat))
df$MaritalStatus          <- as.numeric(as.factor(df$MaritalStatus))

# feature engineering
df$CashbackPerOrder <- df$CashbackAmount / (df$Tenure + 1)
df$ComplaintRate    <- df$Complain / (df$Tenure + 1)
df$RecencyRisk      <- ifelse(df$DaySinceLastOrder > 14, 1, 0)

# verify no remaining NAs
colSums(is.na(df))

# save cleaned data
saveRDS(df, "data/ecommerce_churn_clean.rds")
