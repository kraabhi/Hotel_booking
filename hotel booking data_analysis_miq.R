library(data.table)
dat = fread("108602_July_full_report_MIQ_20180806_122356_785799419 (1).csv", skip = 10)
library(dplyr)

#removing Na's
dat1 =na.omit(dat)
tail(dat1)

#removing blank data where information is not available

dat2 = filter(dat1 , dat1$`Total Revenue Incl. Tax (Local Currency) (number)`!= 0)
tail(dat2)               
summary(dat2)              

filter(dat2, dat2$`Room Daily Rate (Local Currency) - Biz Intelligence Tag (number)`== Inf) 

#changing column names for convinience

dat2 =dat2[,-34]
setnames(dat2,"Transaction Currency (string)" ,"tras_curr")
setnames(dat2,"Total Revenue Incl. Tax (Local Currency) (number)","tot_rev")

#creating new dataframe of currency

currency = data.frame( Curr =c("GBP","EUR", "CHF", "CLP", "HKD", "IDR", "JPY", "MYR", "SGD", "THB", "TWD ", "USD", "CNY"), exch= c(1, 1/1.105, 1/1.258,1/ 857.7,1/10.11,1/ 18497, 1/143.4,1/5.312,1/1.761,1/42.13 , 1/39.57, 1/1.288 , 1/8.97744))

#converting to character as in our Dat2 dataframe currency are character so for joining 

currency$Curr=as.character(currency$Curr)
dat3=dat2 %>% inner_join(currency , by = c("tras_curr"="Curr")) %>% mutate(Revenue_GDP = tot_rev * exch)

#extracting useful variables

dat4 = dat3[,c(2,3,5,9,12:15,17:25,30,31,33,35,42:48)]
dat4 = filter(dat4 , dat4$`Room Rate` != Inf)

#visualization
#rescaling data to have 0 mean and sd of 1
rescale = scale(dat4[,c(21,28)]) 

# Finding appropriate k using with in SS and between ss
max_k <-20 
kmean_withinss <- function(k) {
  cluster <- kmeans(rescale, k)
  return (cluster$tot.withinss)
}
# Run algorithm over a range of k 
wss <- sapply(1:max_k, kmean_withinss)
elbow <-data.frame(1:max_k, wss)
plot(elbow ,type ="l")

source("myfunctions.R")

#kmeans
k_cluster = kmeans(rescale , 3)
centers= k_cluster$centers[k_cluster$cluster,]
distances <- sqrt(rowSums((rescale - centers)^2))
outliers <- order(distances, decreasing=T)[1:5]
Days_arr = rescale[,1]
Rev_GDP =  rescale[,2]

par(mfrow=c(1,1))
plot(Rev_GDP,Days_arr, col= k_cluster$cluster)
points(k_cluster$centers , pch=12 , col = "blue")
legend("topright",legend = c( "instant_high_rev_bookers","instant_low_rev_bookers","Planned_low_rev_bookers"),fill  = c("red","black","green"))


cluster=k_cluster$cluster
cluster =as.data.frame(cluster)
dat5 =cbind(dat4,cluster)
case1 = dat5 %>% filter(cluster==1) 
case2 = dat5 %>% filter(cluster==2) 
case3 = dat5 %>% filter(cluster==3) 
par(mfrow=c(1,1))
hist(dat5$`No. Adult Guests (number)`,col = k_cluster$cluster)
legend("topright",legend = c( "instant_high_rev_bookers","instant_low_rev_bookers","Planned_low_rev_bookers"),fill  = c("red","black","green"))

hist(case1$`Days to Arrival (number)`,col = "red",main = "instant_high_rev_bookers" )
hist(case2$`Days to Arrival (number)`,col = "green",main = "instant_low_rev_bookers" )
hist(case3$`Days to Arrival (number)`,col = "blue",main = "Planned_low_rev_bookers" )
total = sum(dat5$Revenue_GDP)
dat5 %>% group_by(cluster) %>% summarise(Distinct_bookings=n(),mean =mean(Revenue_GDP),sum=sum(Revenue_GDP),Per_contri = (sum/total)*100,avg_days_arrival=mean(`Days to Arrival (number)`))
dat5$`Interaction Channel` = as.factor(dat5$`Interaction Channel`)
 plot(dat5$`Interaction Channel` , col = k_cluster$cluster, main= "Interaction channel effect")
 legend("topright",legend = c( "instant_high_rev_bookers","instant_low_rev_bookers","Planned_low_rev_bookers"),fill  = c("red","black","green"))
 dat5$tras_curr = as.factor(dat5$tras_curr)
 plot(dat5$tras_curr, col = k_cluster$cluster, main= "transaction currency effect")
 legend("topleft",legend = c( "instant_high_rev_bookers","instant_low_rev_bookers","Planned_low_rev_bookers"),fill  = c("red","black","green"))

 dat5$hote 