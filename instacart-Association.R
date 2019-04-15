# R code for the Instacart Challenge by Nick Evangelopoulos (2019), nick.evangelopoulos66@gmail.com
# Based on examples from Kaggle
# Prepared for 7-Eleven, April 15, 2019.

install.packages(c("tidyverse", "data.table", "dplyr", "arules", "arulesViz", "plotly", "IRdisplay"))

library(tidyverse)
library(data.table)
library(dplyr)
library(arules)
library(arulesViz)
library(plotly)
library(IRdisplay)

# 1. Read the csv files into data frames
setwd("C:/RApps")
products <- fread('products.csv')  # also try: products <- read_csv("products.csv")
orders <- fread('orders.csv')
order_products_prior <- fread('order_products__prior.csv')
basket_data = left_join(order_products_prior, products, by='product_id')
head(basket_data)

# 2. Aggregate at the order level (3.2M orders)
basket_data = group_by(basket_data, order_id)
basket_data = summarise(basket_data,items=as.vector(list(product_name)))
basket_data = left_join(basket_data, orders, by='order_id')
basket_data = select(basket_data, order_id, user_id, items)

# 3. Converting the data frame to a transaction set
transactions=as(basket_data$items, 'transactions') # this takes about 2 minutes
dim(transactions)

# 4. Mine associations with Apriori
transac <- head(transactions,100000)  # use a sample of 100K transactions
ct <- length(transactions)
cts <- length(transac)
i <- c(5,10,15,20)

for (val in i){
  print(val/cts)
myrules=apriori(transac, list(support=val/cts, confidence=0.4, maxlen= 5))
print(summary(myrules))
}
inspect(myrules[1:10])

# Graph that shows associations among the top 20 products: 
plot(myrules[1:20],
method = "graph",
control = list(type = "items"))




