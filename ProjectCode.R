salesdata = read.csv("C:\\Users\\Surabhi\\Desktop\\DataMining\\Project\\sales_train\\daily_sales.csv")
itemcategory = read.csv("C:\\Users\\Surabhi\\Desktop\\DataMining\\Project\\items.csv",
                        colClasses = c("NULL",NA,NA))

install.packages("ggplot2")
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplots2")
install.packages("FNN")

library("lubridate")
library("dplyr")
library("gmodels")
library("ggplot2")
library("FNN")


##descriptive statistics
summary(salesdata)
nrow(salesdata[salesdata$item_cnt_day<0,])
nrow(salesdata)

##merging data by "item_id"
salescategory = merge(salesdata,itemcategory,by = "item_id")
summary(salescategory)

salescategory["totalprice"] = salescategory$item_price*salescategory$item_cnt_day
salescategory$date = gsub("[.]","/",salescategory$date)
salescategory$date = dmy(salescategory$date)
salescategory["year"] = year(salescategory$date)
salescategory["day"] = day(salescategory$date)
salescategory["month"] = month(salescategory$date)

summary(salescategory)

##plot of negative count values
neg = salescategory[salescategory$item_cnt_day<0,"item_cnt_day"]*-1
date_block = salescategory[salescategory$item_cnt_day<0,"date_block_num"]
dataplt = data.frame(neg,date_block)

g = ggplot() + 
  geom_point(data = dataplt,color = "blue",aes(x = date_block, y = neg)) +
  xlab('date_block_num') +
  ylab('negative item counts')+
  ggtitle("Plot of date_block_num by negative item counts")
print(g)


#getting dates with negative count days
negativecount = salescategory[salescategory$item_cnt_day<0,
                              c("date","item_id","item_category_id","item_cnt_day","shop_id")]


##newdata without the rows not accounted for
newdata = salescategory
negcount = negativecount

##To get the dates that do not satify the condition of returns
##Warning: This patch of code might take 10 mins to run
count = 0
count1 = 0
for(row in row.names.data.frame(negativecount))
{
  counts = salescategory[ salescategory$date < negativecount[row,"date"] &
                          salescategory$date > negativecount[row,"date"]-31 & 
                          salescategory$item_id == negativecount[row,"item_id"] &
                          salescategory$item_category_id == negativecount[row,"item_category_id"]&
                          salescategory$shop_id == negativecount[row,"shop_id"]&
                         salescategory$item_cnt_day > 0,"item_cnt_day"]
  
  month = month(negativecount[row,"date"])
  if(sum(counts)< -1*negativecount[row,"item_cnt_day"] & month != 1)
  {
    count = count + 1
  }
  else{
    count1=count1+1
    negcount = negcount[-c(which(row.names(negcount) == row)),]
  }
  
}

#contains the rows that have to be removed from the dataset
nrow(negcount)
View(negcount)

#removing rows from salescategory which are in negativecount
newdata = newdata[-as.numeric(row.names(negcount)),]


####Missing values, inconsistent data
##imputing the row with item_price = -1
attach(newdata)
newdata[newdata$item_price == -1,]
newdata[newdata$item_id == 2973 & newdata$shop_id ==32,]
###since the mode of the prices for item category are 2499
###replacing the negative entry with 2499
newdata[item_price== -1,"item_price"] = 2499
#correctig the value of totalprice
newdata$totalprice = newdata$item_price*newdata$item_cnt_day
summary(newdata)
newdata[newdata$item_cnt_day < 0,]
newdata[item_price == 0.07,] #similar values , therefore can be considered
newdata[newdata$item_id == 11864,]
newdata[newdata$item_id == 11865,]
newdata[newdata$item_price == 307980,] #only one item therefore can be considered
newdata[newdata$item_id == 6066,]




##plotting variation by month
newdata$month = factor(newdata$month)

#2013
datasub13 = newdata[newdata$year == 2013,]
attach(datasub13)
class(datasub13$month)
group = group_by(datasub13,month)
year13 = summarise(group,sum(totalprice)/1000000)

#2014
datasub14 = newdata[newdata$year == 2014,]
attach(datasub14)
class(datasub14$month)
group = group_by(datasub14,month)
year14 = summarise(group,sum(totalprice)/1000000)

#2015
datasub15 = newdata[newdata$year == 2015,]
attach(datasub15)
class(datasub15$month)
group = group_by(datasub15,month)
year15 = summarise(group,sum(totalprice)/1000000)

ggplot(data = year13,aes(x = year13$month, y = year13$`sum(totalprice)/1e+06`))+geom_point(color = "red")

p = ggplot() + 
  geom_point(data = year13, aes(x = year13$month, y = year13$`sum(totalprice)/1e+06`,color = "blue")) +
  geom_point(data = year14, aes(x = year14$month, y = year14$`sum(totalprice)/1e+06`,color = "green")) +
  geom_point(data = year15, aes(x = year15$month, y = year15$`sum(totalprice)/1e+06`,color = "red"))+
  xlab('Month') +
  ylab('total sales in the nearest million')+
  ggtitle("Plot of sales by month in each year")+
  scale_colour_manual(name = "",
                      values =c("blue"="blue","red"="red","green"="green"),
                      labels = c('2013','2014','2015'))
  
print(p)

attach(newdata)

#assigning train and test data 
##taking all month except october 2015
train = newdata[newdata$date_block_num != 33,]
##October 2015 data
test = newdata[newdata$date_block_num == 33,]
View(train)

g = ggplot() + 
  geom_line(data = testshopsales, aes(x = testshopsales$shop_id, y = testshopsales$`sum(totalprice)`/1000000,color = "blue")) +
  geom_line(data = testshopsales, aes(x = testshopsales$shop_id, y = testshopsales$`sum(predicted)`/1000000,color = "green")) +
  xlab('Shop_id') +
  ylab('total sales in the nearest million')+
  ggtitle("Plot of sales by month in each year")+
  scale_colour_manual(name = "",
                      values =c("blue"="blue","green"="green"),
                      labels = c('actual','predicted'))
print(g)


##knn by aggregating the data for each month by item id
##grouping the data by shopd id and date block num
groupshopdatablock = group_by(newdata,shop_id,item_id,item_category_id,date_block_num,month,year)
shopdatablock = summarise(groupshopdatablock,sum(totalprice))
shopdatablock = as.data.frame(shopdatablock)
length(boxplot(shopdatablock$`sum(totalprice)`)$out)

attach(shopdatablock)
shopdatablock["cluster"] = rep(1,1608881)

##getting ACF and PACF plots
tsdata = ts(shopdatablock$`sum(totalprice)`)
df = shopdatablock[,c(4,7)]
sum(df$`sum(totalprice)`)
attach(df)
df$date_block_num = as.factor(df$date_block_num)
df[,"price"] = df$`sum(totalprice)`
df = df[,-2]
groups = group_by(df,date_block_num)
View(groups)
grouped = summarise(groups,sum(price))
View(grouped)
tsdata = ts(grouped$`sum(price)`)
acf(tsdata,main = "ACF plot of data grouped by date_block_num")
pacf(tsdata, main = "PACF of data grouped by date_block_num")
ts.plot(tsdata)
diff1 = diff(tsdata)
diff2 = diff(diff1)
ts.plot(diff2)
ts.plot(diff1)

df1 = shopdatablock[,c(4,5,7)]
attach(df1)
df1[,"price"] = df1$`sum(totalprice)`
df1 = df1[,-3]
groups1 = group_by(df1,month)
grouped1 = summarise(groups1,sum(price))
tsdata = ts(grouped1$`sum(price)`)
ts.plot(tsdata)
diff1 = diff(tsdata)
ts.plot(diff1)
diff2 = diff(diff1)
ts.plot(diff2)
acf(diff2,main = "ACF plot of data grouped by month")
pacf(diff2, main = "PACF plot of data grouped by month" )

##assigning cluster numbers

shopdatablock[shopdatablock$month == 1 | shopdatablock$month == 2|shopdatablock$month == 3,"cluster"] = 2
shopdatablock[shopdatablock$month == 4|
                shopdatablock$month == 5|shopdatablock$month == 6|
                shopdatablock$month ==7|shopdatablock$month == 8,"cluster"] = 3
shopdatablock[shopdatablock$month == 9|shopdatablock$month == 10,"cluster"] = 4

View(shopdatablock)
train1 = shopdatablock[date_block_num !=33,]
test1 = shopdatablock[date_block_num == 33,]
test1["totalprice"] = test1$`sum(totalprice)`
train1["totalprice"] = train1$`sum(totalprice)`


test1 = test1[,-c(6)]
train1 = train1[,-c(6)]


g = ggplot() + 
  geom_line(data = testshopsales1, aes(x = testshopsales1$shop_id, y = testshopsales1$`sum(ltotalprice)`/1000000,color = "blue")) +
  geom_line(data = testshopsales1, aes(x = testshopsales1$shop_id, y = testshopsales1$`sum(predicted)`/1000000,color = "green")) +
  xlab('Shop_id') +
  ylab('total sales in the nearest million')+
  ggtitle("Plot of sales by month in each year")+
  scale_colour_manual(name = "",
                      values =c("blue"="blue","green"="green"),
                      labels = c('actual','predicted'))
print(g)




train3 = train1[,c("item_id","shop_id","date_block_num")]
test3 = test1[,c("item_id","shop_id","date_block_num")]
trainoutcome = train1[,c("totalprice")]
testoutcome = test1[,c("totalprice")]
set.seed(12345)
test3$date_block_num = as.numeric(test3$date_block_num)
train3$date_block_num = as.numeric(train3$date_block_num)


start = Sys.time()
model3 = knn.reg(train = train3,y = trainoutcome, test = test3, k=50)
end = Sys.time()
print(end-start)
outcome = data.frame(model3$pred,testoutcome)
MSE = mean((testoutcome - model3$pred)^2)
RMSE = MSE^(1/2)
MSE
RMSE


View(data.frame(test3,outcome))

attach(test3)
test3data = data.frame(test3,model3$pred,testoutcome)
test3shop = group_by(test3data,shop_id)
test3plt = summarise(test3shop,sales = sum(testoutcome),pred = sum(model3.pred))
View(test3plt)

g = ggplot() + 
  geom_line(data = test3plt, aes(x = test3plt$shop_id, y = test3plt$sales/1000000,color = "blue")) +
  geom_line(data = test3plt, aes(x = test3plt$shop_id, y = test3plt$pred/1000000,color = "red")) +
  xlab('Shop_id') +
  ylab('total sales in the nearest million')+
  ggtitle("Plot of sales by month in each year")+
  scale_colour_manual(name = "",
                      values =c("blue"="blue","red"="red"),
                      labels = c('actual','predicted'))
print(g)


test3item = group_by(test3,item_id)
test3pltitem = summarise(test3item,sales = sum(testoutcome),pred = sum(model3.pred))

g = ggplot() + 
  geom_line(data = test3pltitem, aes(x = test3pltitem$item_id, y = test3pltitem$sales/1000000,color = "blue")) +
  geom_line(data = test3pltitem, aes(x = test3pltitem$item_id, y = test3pltitem$pred/1000000,color = "red")) +
  xlab('item_id') +
  ylab('total sales in the nearest million')+
  ggtitle("Plot of sales by month in each year")+
  scale_colour_manual(name = "",
                      values =c("blue"="blue","red"="red"),
                      labels = c('actual','predicted'))
print(g)



##training XGBoost

install.packages("xgboost")
library(xgboost)
train4 = data.frame(lapply(train3,as.numeric))
test4 = data.frame(lapply(test3,as.numeric))
class(train4)
View(train4)
start = Sys.time()
bst = xgboost(data = data.matrix(train4),label = trainoutcome, max_depth = 3, eta =0.1, nthread = 2,
              nrounds = 100, objective = "reg:linear")
end = Sys.time()
print(end-start)
pred = predict(bst,data.matrix(test4))
length(pred)
nrow(test4)
testoutcome
MSE = mean((pred - testoutcome)^2)
RMSE = (MSE)^(1/2)
MSE
RMSE
View(data.frame(test4,pred,testoutcome))

test3pred = data.frame(test3,pred,testoutcome)
attach(test3pred)
View(test3pred)
test3shop = group_by(test3pred,shop_id)
test3plt = summarise(test3shop,sales = sum(testoutcome),pred = sum(pred))
View(test3plt)

g = ggplot() + 
  geom_line(data = test3plt, aes(x = test3plt$shop_id, y = test3plt$sales/10000000,color = "blue")) +
  geom_line(data = test3plt, aes(x = test3plt$shop_id, y = test3plt$pred/10000000,color = "red")) +
  xlab('Shop_id') +
  ylab('total sales in the nearest million')+
  ggtitle("Plot of sales by month in each year")+
  scale_colour_manual(name = "",
                      values =c("blue"="blue","red"="red"),
                      labels = c('actual','predicted'))
print(g)

test3item = group_by(test3pred,item_id)
test3plt = summarise(test3item,sales = sum(testoutcome),pred = sum(pred))

View(test3plt)

g = ggplot() + 
  geom_line(data = test3plt, aes(x = test3plt$item_id, y = test3plt$sales/10000000,color = "blue")) +
  geom_line(data = test3plt, aes(x = test3plt$item_id, y = test3plt$pred/10000000,color = "red")) +
  xlab('Shop_id') +
  ylab('total sales in the nearest million')+
  ggtitle("Plot of sales by month in each year")+
  scale_colour_manual(name = "",
                      values =c("blue"="blue","red"="red"),
                      labels = c('actual','predicted'))
print(g)

###getting confidence interval for knn reg
shopdatablock[,"totalprice"] = shopdatablock[,"sum(totalprice)"]
shopdatablock = shopdatablock[,-7]
crossRMSE = 1:10
count = 0
for(i in 23:32){
  
  train = shopdatablock[shopdatablock$date_block_num < i,c("item_id","shop_id","date_block_num")]
  test = shopdatablock[shopdatablock$date_block_num == i,c("item_id","shop_id","date_block_num")]
  trainoutcome = shopdatablock[shopdatablock$date_block_num < i,c("totalprice")]
  testoutcome = shopdatablock[shopdatablock$date_block_num == i,c("totalprice")]
  model = knn.reg(train = train,y = trainoutcome, test = test, k=50)
  MSE = mean((testoutcome - model$pred)^2)
  RMSE = MSE^(1/2)
  MSE
  RMSE
  count = count + 1
  crossRMSE[count] = RMSE
  
}

conf95 = ci(crossRMSE,confidence = 0.95)
conf90 = ci(crossRMSE,confidence = 0.90)
conf80 = ci(crossRMSE,confidence = 0.80)
conf95
conf90
conf80


###getting confidence interval for XGboost
crossRMSE = 1:10
count = 0
for(i in 23:32){
  
  train = shopdatablock[shopdatablock$date_block_num < i,c("item_id","shop_id","date_block_num")]
  test = shopdatablock[shopdatablock$date_block_num == i,c("item_id","shop_id","date_block_num")]
  train = data.frame(lapply(train,as.numeric))
  test = data.frame(lapply(test,as.numeric))
  trainoutcome = shopdatablock[shopdatablock$date_block_num < i,c("totalprice")]
  testoutcome = shopdatablock[shopdatablock$date_block_num == i,c("totalprice")]
  
  model = xgboost(data = data.matrix(train),label = trainoutcome, max_depth = 3, eta =0.1, nthread = 2,
                  nrounds = 100, objective = "reg:linear")
  pred = predict(bst,data.matrix(test))
  MSE = mean((testoutcome - pred)^2)
  RMSE = MSE^(1/2)
  MSE
  RMSE
  count = count + 1
  crossRMSE[count] = RMSE
  
}

conf95 = ci(crossRMSE,confidence = 0.95)
conf90 = ci(crossRMSE,confidence = 0.90)
conf80 = ci(crossRMSE,confidence = 0.80)
conf95
conf90
conf80

