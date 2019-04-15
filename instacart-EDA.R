# R code for the Instacart Challenge by Nick Evangelopoulos (2019), nick.evangelopoulos66@gmail.com
# Based on examples from Kaggle

install.packages(c("data.table", "dplyr", "ggplot2", "knitr", "stringr", "DT", "treemap"))

library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)
library(treemap)

setwd("C:/RApps")
aisles <- fread('aisles.csv')
departments <- fread('departments.csv')
products <- fread('products.csv')
orders <- fread('orders.csv')
order_products <- fread('order_products__train.csv')
order_products_prior <- fread('order_products__prior.csv')

# Peek at the dataset
kable(head(products,10))
glimpse(products)
kable(head(orders,12))
glimpse(orders)
kable(head(order_products,10))
glimpse(order_products_prior)

# Recode variables to convert character variables to factors
orders <- orders %>% mutate(order_hour_of_day = as.numeric(order_hour_of_day), eval_set = as.factor(eval_set))
products <- products %>% mutate(product_name = as.factor(product_name))
aisles <- aisles %>% mutate(aisle = as.factor(aisle))
departments <- departments %>% mutate(department = as.factor(department))

# When do people order? Hour of the Day [takes 6 sec to plot]
orders %>% ggplot(aes(x=order_hour_of_day)) + geom_histogram(stat="count",fill="blue")

# When do people order? Day of the Week
orders %>% ggplot(aes(x=order_dow)) + geom_histogram(stat="count",fill="blue")

# When do people order? Days since prior order
orders %>% ggplot(aes(x=days_since_prior_order)) + geom_histogram(stat="count",fill="blue")

# How many prior orders?
orders %>% filter(eval_set=="prior") %>% count(order_number) %>% ggplot(aes(order_number,n)) + geom_line(color="blue", size=1)+geom_point(size=2, color="blue")
# How many train orders?
orders %>% filter(eval_set=="train") %>% count(order_number) %>% ggplot(aes(order_number,n)) + geom_line(color="blue", size=1)+geom_point(size=2, color="blue")
# How many test orders?
orders %>% filter(eval_set=="test") %>% count(order_number) %>% ggplot(aes(order_number,n)) + geom_line(color="blue", size=1)+geom_point(size=2, color="blue")

# How many items do people buy?
order_products %>% group_by(order_id) %>% summarize(n_items = last(add_to_cart_order)) %>% ggplot(aes(x=n_items))+ geom_histogram(stat="count",fill="blue") + geom_rug()+ coord_cartesian(xlim=c(0,80))

order_products_prior %>% group_by(order_id) %>% summarize(n_items = last(add_to_cart_order)) %>% ggplot(aes(x=n_items))+ geom_histogram(stat="count",fill="blue") + geom_rug() + coord_cartesian(xlim=c(0,80))

# Products sold most often
tmp <- order_products %>%
group_by(product_id) %>%
summarize(count = n()) %>%
top_n(10, wt = count) %>%
left_join(select(products,product_id,product_name),by="product_id") %>%
arrange(desc(count))
kable(tmp)
tmp %>%
ggplot(aes(x=reorder(product_name,-count), y=count))+
geom_bar(stat="identity",fill="blue")+
theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())

# How often people reorder the same product
tmp <- order_products %>%
group_by(reordered) %>%
summarize(count = n()) %>%
mutate(reordered = as.factor(reordered)) %>%
mutate(proportion = count/sum(count))
kable(tmp)
tmp %>%
ggplot(aes(x=reordered,y=count,fill=reordered))+
geom_bar(stat="identity")+
theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())

# Products most often reordered
tmp <-order_products %>%
group_by(product_id) %>%
summarize(proportion_reordered = mean(reordered), n=n()) %>%
filter(n>40) %>%
top_n(10,wt=proportion_reordered) %>%
arrange(desc(proportion_reordered)) %>%
left_join(products,by="product_id")
kable(tmp)

tmp %>%
ggplot(aes(x=reorder(product_name,-proportion_reordered), y=proportion_reordered))+
geom_bar(stat="identity",fill="blue")+
theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+coord_cartesian(ylim=c(0.85,0.95))

# Products placed in the cart first
tmp <- order_products %>%
group_by(product_id, add_to_cart_order) %>%
summarize(count = n()) %>% mutate(pct=count/sum(count)) %>%
filter(add_to_cart_order == 1, count>10) %>%
arrange(desc(pct)) %>%
left_join(products,by="product_id") %>%
select(product_name, pct, count) %>%
ungroup() %>%
top_n(10, wt=pct)
kable(tmp)

tmp %>%
ggplot(aes(x=reorder(product_name,-pct), y=pct))+
geom_bar(stat="identity",fill="blue")+
theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+coord_cartesian(ylim=c(0.4,0.7))

# Probability for a product to be reordered vs. the number of times the product was ordered. 
order_products %>%
left_join(orders,by="order_id") %>%
group_by(days_since_prior_order) %>%
summarize(mean_reorder = mean(reordered)) %>%
ggplot(aes(x=days_since_prior_order,y=mean_reorder))+
geom_bar(stat="identity",fill="blue")

# Organic vs. non-organic products
products <- products %>% mutate(organic=ifelse(str_detect(str_to_lower(products$product_name),'organic'),"organic","non-organic"), organic= as.factor(organic))
tmp <- order_products %>%
left_join(products, by="product_id") %>%
group_by(organic) %>%
summarize(count = n()) %>%
mutate(proportion = count/sum(count))
kable(tmp)

# Treemap to show how many unique products are offered in each department/aisle
tmp <- products %>%
group_by(department_id, aisle_id) %>%
summarize(n=n())
tmp <- tmp %>%
left_join(departments,by="department_id")
tmp <- tmp %>%
left_join(aisles,by="aisle_id")

tmp2<-order_products %>%
group_by(product_id) %>%
summarize(count=n()) %>%
left_join(products,by="product_id") %>%
ungroup() %>%
group_by(department_id,aisle_id) %>%
summarize(sumcount = sum(count)) %>%
left_join(tmp, by = c("department_id", "aisle_id")) %>%
mutate(onesize = 1)
treemap(tmp,index=c("department","aisle"),vSize="n",title="",palette="Set3",border.col="#FFFFFF")

# Treemap to show how often products from the department/aisle are sold
treemap(tmp2,index=c("department","aisle"),vSize="sumcount",title="",palette="Set3",border.col="#FFFFFF")

# Customers who reorder the same products (re-order proportion = 1.0)
tmp <- order_products_prior %>%
group_by(order_id) %>%
summarize(m = mean(reordered),n=n()) %>%
right_join(filter(orders,order_number>2), by="order_id")
tmp2 <- tmp %>%
filter(eval_set =="prior") %>%
group_by(user_id) %>%
summarize(n_equal = sum(m==1,na.rm=T), percent_equal = n_equal/n()) %>%
filter(percent_equal == 1) %>%
arrange(desc(n_equal))
datatable(tmp2, class="table-condensed", style="bootstrap", options = list(dom = 'tp'))
kable(tmp2)

# The customer with the strongest habit
uniqueorders <- filter(tmp, user_id == 99753)$order_id
tmp <- order_products_prior %>%
filter(order_id %in% uniqueorders) %>%
left_join(products, by="product_id")
datatable(select(tmp,-aisle_id,-department_id,-organic), style="bootstrap", class="table-condensed", options = list(dom = 'tp'))
kable(tmp)

# The order for Customer 99753 in the training set 
tmp <- orders %>%
filter(user_id==99753, eval_set == "train")
tmp2 <- order_products %>%
filter(order_id == tmp$order_id) %>%
left_join(products, by="product_id")

datatable(select(tmp2,-aisle_id,-department_id,-organic), style="bootstrap", class="table-condensed", options = list(dom = 't'))


