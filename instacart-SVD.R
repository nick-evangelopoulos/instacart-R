# R code for the Instacart Challenge by Nick Evangelopoulos (2019), nick.evangelopoulos66@gmail.com
# Based on examples from Kaggle

install.packages(c("tidyverse", "data.table", "dplyr", "psych"))

library(tidyverse)
library(data.table)
library(dplyr)
library(psych)	

# Part 1: Read the csv files into data frames
setwd("C:/RApps")
products <- read_csv("products.csv")  # also try: products <- fread('products.csv')
orders <- read_csv("orders.csv")
order_products_prior <- fread('order_products__prior.csv')

# Part 2: Compile a product-by-hour crosstabulation
crosstab_data = left_join(order_products_prior, products, by='product_id')
crosstab_data <- crosstab_data %>%
  left_join(orders, by='order_id') %>%
  select (order_id, product_id, order_dow, order_hour_of_day, product_name)
head(crosstab_data)
crosstab_data2 <- crosstab_data %>%
  count(product_id, order_dow, order_hour_of_day) %>%
  mutate(order_hour_of_week = order_dow * 24 + order_hour_of_day) %>%
  select(product_id, order_hour_of_week, n)
head(crosstab_data2)
crosstab <- crosstab_data2 %>%
  spread(key = order_hour_of_week, value = n, fill = 0)
str(crosstab)

# Part 3: Singular Value Decomposition
crosstab$product_id <- NULL
X <- as.matrix(crosstab)
X.svd <- svd(X)
d <- diag(X.svd$d)
plot(1:length(X.svd$d), X.svd$d)

u <- X.svd$u
v <- X.svd$v
k <- 10  # number of factors to retain
uk <- as.matrix(u[, 1:k])
vk <- as.matrix(v[, 1:k])
sk <- as.matrix(d[1:k, 1:k])
Xhat <- uk %*% sk %*% t(vk)  #  Xhat = U D V'

LHour <- vk %*% sk
LProd <- uk %*% sk
write.csv(LHour, file = "LHour_10f.csv")
write.csv(LProd, file = "LProd_10f.csv")
RotLHour <- varimax(LHour, normalize = TRUE, eps = 1e-9)
LProd <- fa.sort(LProd %*% as.matrix(RotLHour$rotmat))
LHour <- fa.sort(LHour %*% as.matrix(RotLHour$rotmat))
write.csv(LHour, file = "LHour-Rot_10f.csv")
write.csv(LProd, file = "LProd-Rot_10f.csv")




