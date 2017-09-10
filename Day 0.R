data = read.table("00-prices.txt", FALSE, sep=" ", col.names=c("id", "price", "location", "extras"))
sum(data$location=="Nullarbor")

#######################

data$aunt = data$extras - 500
data$aunt[data$aunt<0] = 0
data$cost = data$aunt + data$price

library(dplyr)
orData = arrange(data, cost)

