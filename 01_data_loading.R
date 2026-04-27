# E-Commerce Customer Churn Prediction
# Data Loading & Initial Inspection

library(readxl)
library(dplyr)
library(ggplot2)

df <- read_excel("data/ecommerce_churn.xlsx")

dim(df)
names(df)
str(df)
head(df)
summary(df)

# check missing values
colSums(is.na(df))

# churn distribution
table(df$Churn)
prop.table(table(df$Churn))
