library(dplyr)

data = read.table("01-holidays.txt", FALSE, sep=" ", col.names=c("ID", "price", "location", "nights"))
sum(data$price < 1200)

##################

cheapData = data[data$price < 1200,]
cheapData$weights = 1.0

cheapData[cheapData$location=="Almaty", "weights"] = 2.0
cheapData[cheapData$location=="Brorfelde", "weights"] = 0.9
cheapData[cheapData$location=="Estacada", "weights"] = 0.4
cheapData[cheapData$location=="Jayuya", "weights"] = 0.6
cheapData[cheapData$location=="Karlukovo", "weights"] = 2.2
cheapData[cheapData$location=="Morgantown", "weights"] = 2.9
cheapData[cheapData$location=="Nordkapp", "weights"] = 1.5
cheapData[cheapData$location=="Nullarbor", "weights"] = 2.2
cheapData[cheapData$location=="Puente-Laguna-Garzonkuala-Penyu", "weights"] = 0.4
cheapData[cheapData$location=="Uzupis", "weights"] = 0.9

cheapData$weighting = cheapData$weights * (cheapData$nights / cheapData$price)
orderedData = arrange(cheapData, desc(weighting))

###############

#
# for(x in 1:length(prices)) {
#     if((total + prices[1+x]) > 1200) {
#         break
#     }
#     total = total + prices[1+x]
#     holCount = holCount + 1
# }
#
# print(holCount)
