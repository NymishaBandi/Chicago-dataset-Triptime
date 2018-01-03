library(RecordLinkage)
library(lubridate)
library(reshape2)
library(tree)
library("PerformanceAnalytics")
library(randomForest)
library(Metrics)
library("adabag")
library(gbm)
library(glmnet)
library(ggplot2)
library(car)

#load data
taxi_1 <- read_csv("~/Desktop/Course Work/Adv. Stats/Project/Taxi/taxi_1.csv")

#removing observations which don't have pickup and dropoff community area
taxi_imp = taxi_1[-which(is.na(taxi_1$pickup_community_area)), ]
taxi_imp = taxi_imp[-which(is.na(taxi_imp$dropoff_community_area)), ]
summary(taxi_imp)

#remove observations with 0 trip_miles, trip_seconds
taxi_imp = subset(taxi_imp, taxi_imp$trip_miles!=0)
taxi_imp = subset(taxi_imp, taxi_imp$trip_seconds!=0)
summary(taxi_imp)

#unique encoding Taxi_ID and Trip_ID
taxi_imp$trip_ID<-match(taxi_imp$trip_ID,unique(taxi_imp$trip_ID))
taxi_imp$taxi_ID<-match(taxi_imp$taxi_ID,unique(taxi_imp$taxi_ID))


#remove outliers in trip time
hist(taxi_imp$trip_seconds, main = "time Variable Histogram", xlab = "time")
taxi_Outlier = subset(taxi_imp, taxi_imp$trip_seconds<(mean(taxi_imp$trip_seconds)+1.5*sd(taxi_imp$trip_seconds)))
hist(taxi_Outlier$trip_seconds, main = "time Variable Histogram", xlab = "time")
summary(taxi_Outlier)

#remove outliers in trip miles
hist(taxi_Outlier$trip_miles, main = "dist Variable Histogram", xlab = "dist")
taxi_Outlier = subset(taxi_Outlier, taxi_Outlier$trip_miles<(mean(taxi_Outlier$trip_miles)+1.5*sd(taxi_Outlier$trip_miles)))
hist(taxi_Outlier$trip_miles, main = "dist Variable Histogram", xlab = "dist")
summary(taxi_Outlier)

# add speed of travel
taxi_Outlier["Speed"]<-NA
taxi_Outlier$Speed<-taxi_Outlier$trip_miles*60*60/taxi_Outlier$trip_seconds

#remove unusually high speeds data
hist(taxi_Outlier$Speed, main = "Speed Variable Histogram", xlab = "Speed")
taxi_Outlier = subset(taxi_Outlier, taxi_Outlier$Speed<60)
hist(taxi_Outlier$Speed, main = "Speed Variable Histogram", xlab = "Speed")

#Adding weekday as a column
taxi_Outlier$trip_start_timestamp=mdy_hms(taxi_Outlier$trip_start_timestamp)+ 6*60*60
taxi_Outlier["Weekday"]<-NA
taxi_Outlier$Weekday <- ifelse(weekdays(as.Date(taxi_Outlier$trip_start_timestamp,format = "%Y-%m-%d %H:%M")) %in% c("Saturday", "Sunday"), 0, 1)

#getting hours
taxi_Outlier$time_hrs <- as.numeric(format(as.POSIXlt(taxi_Outlier$trip_start_timestamp, format="%Y-%m-%d %H:%M"), format="%H"))
hist(taxi_Outlier$time_hrs,mean(taxi_Outlier$trip_seconds))

#calculating avg trip time by hour of the day
plot(aggregate(taxi_Outlier,list(taxi_Outlier$time_hrs),mean)$Group.1,aggregate(taxi_Outlier,list(taxi_Outlier$time_hrs),mean)$trip_seconds,type="l",xlab="Hr of the day", ylab="Avg trip time")
lines(x = c(0,650), y = c(650,700),col="blue")

#Splitting time into 5 bins with cutoff at 650secs avg time
#the bins obtained are:0-3,4-10,11-12,13-22,23
taxi_Outlier["TimeStamp"]<-NA
taxi_Outlier$TimeStamp<-ifelse((as.numeric(taxi_Outlier$time_hrs)>=0) & (as.numeric(taxi_Outlier$time_hrs)<=3), 1, ifelse((as.numeric(taxi_Outlier$time_hrs)>3) & (as.numeric(taxi_Outlier$time_hrs)<=10), 2, ifelse((as.numeric(taxi_Outlier$time_hrs)>10) & (as.numeric(taxi_Outlier$time_hrs)<=12), 3, ifelse((as.numeric(taxi_Outlier$time_hrs)>12) & (as.numeric(taxi_Outlier$time_hrs)<=22), 4, ifelse((as.numeric(taxi_Outlier$time_hrs)>22) & (as.numeric(taxi_Outlier$time_hrs)<=23), 5, 0)))))

#converting all columns to numeric data type
#for(i in 1:ncol(taxi_Outlier)){
#  taxi_Outlier[,i]<-as.numeric(unlist(taxi_Outlier[,i]))}

#removing Census tract info, company and converting all columns to numeric; removing rows with NA
taxi_Clean = subset(taxi_Outlier, select = -c(pickup_census_tract,dropoff_census_tract,trip_end_timestamp,trip_start_timestamp,Company,Fare,Tips,Tolls,Extras,trip_total,payment_type,pickup_latitude,pickup_longitude,dropoff_latitude,dropoff_longitude,time_hrs) )


#Let us consider a subset of the data for analysis. Only rides on weekends and between 1PM-10PM
Taxi_New=subset(taxi_Clean,taxi_Clean$TimeStamp==4)
Taxi_New_Week=subset(Taxi_New,Taxi_New$Weekday==0)
Taxi_New_Week = subset(Taxi_New_Week, select = -c(TimeStamp,Weekday))

summary(Taxi_New_Week)
summary(taxi_Clean)
str(taxi_Clean)

#----------EA-------------------
library(ggmap)
library(Rcpp)


#Exploratory analysis:
#Distribution of ride duration
ggplot(Taxi_New_Week, aes(x=factor(trip_seconds), y=trip_seconds)) + stat_summary(fun.y="length", geom="bar")+ labs(y = "Count of trips",x="Trip seconds")
#Exploratory analysis of average trip time with respect to day of the week
ggplot(taxi_Clean, aes(x=factor(TimeStamp), y=trip_seconds)) + stat_summary(fun.y="mean", geom="bar")+ labs(y = "Trip time avg",x="Time bins")

sum(is.na(taxi_Dem))
taxi_Dem=subset(taxi_Outlier,select=-c(pickup_census_tract,dropoff_census_tract,trip_end_timestamp,trip_start_timestamp,Fare,Tips,Tolls,Extras,trip_total,payment_type,time_hrs,Speed,trip_seconds,trip_miles,Company))
taxi_Dem["Rides"]<-NA

#number of taxi rides by pickup_community_area, dropoff_community_area, Weekday, Time stamp
taxi_Dem1 <- aggregate(cbind(count = taxi_ID) ~ pickup_community_area+dropoff_community_area+Weekday+TimeStamp, 
                       data = taxi_Dem, 
                       FUN = function(x){NROW(x)})

#exploratory analysis
#demand by pickup community and weekday
ggplot(taxi_Dem1, aes(x=pickup_community_area, y=count,group=factor(Weekday), color=factor(Weekday))) + stat_summary(fun.y="length", geom="line")+ labs(y = "Count of trips",x="Pickup Community")
#demand by pickup community and Time stamp
ggplot(taxi_Dem1, aes(x=pickup_community_area, y=count,group=factor(TimeStamp), color=factor(TimeStamp))) + stat_summary(fun.y="length", geom="line")+ labs(y = "Count of trips",x="Pickup Community")
#demand by Time stamp and weekday
ggplot(taxi_Dem1, aes(x=TimeStamp, y=count, color=factor(Weekday))) + stat_summary(fun.y="length", geom="line")+ labs(y = "Count of trips",x="TimeStamp")

map <- get_map(location = 'Chicago',zoom=10)
points<-ggmap(map)+ geom_point(aes(x = pickup_longitude, y = pickup_latitude,size=fare), data = taxi_fare1) 
points

ggmap(map)+stat_density2d(aes(x = pickup_longitude, y = pickup_latitude, fill = count,alpha=..level..), bins = 10, geom = "polygon", data = taxi_Dem1) +
  scale_fill_gradient(low = "black", high = "red")+
  ggtitle("Demand Density in Chicago")

head(sort(taxi_Dem1$count,decreasing=TRUE), n = 50)

Maxtravel=head(taxi_Dem1[order(taxi_Dem1$count, decreasing=TRUE), ], 50)

Maxtravel=subset(taxi_Dem1,taxi_Dem1$count==max(taxi_Dem1$count))
map <- get_map(location = 'Chicago',zoom=14)
Maxpoint<-ggmap(map)+ geom_point(aes(x = dropoff_longitude, y = pickup_latitude,size=count), data = Maxtravel)
Maxpoint

taxi_Dem2<-taxi_Dem
for(i in 1:ncol(taxi_Dem)){
  taxi_Dem[,i]<-as.numeric(unlist(taxi_Dem[,i]))}

na.omit(taxi_Dem1)

#----------------Regression models--------------

for(i in 1:ncol(Taxi_New_Week)){
  Taxi_New_Week[,i]<-as.numeric(unlist(Taxi_New_Week[,i]))}

#Variable importance
#finding correlation
cor(Taxi_New_Week)

#split data with 60% split ratio
set.seed (1)
train = sample(nrow(Taxi_New_Week), nrow(Taxi_New_Week)*0.6)
taxi_Clean.train = Taxi_New_Week[train, ]
taxi_Clean.test = Taxi_New_Week[-train, ]
rm(train)

#the correlation of trip_seconds is the most with trip_miles, speed, dropoff_community_area
#plot gives the relationship between all the variables
plot(taxi_Clean.train)

#forward selection
FullModel <- lm(taxi_Clean.train$trip_seconds ~ ., data=taxi_Clean.train)         
EmptyModel <- update(FullModel, . ~ 1)
forwards = step(EmptyModel, scope=list(lower=.~1,upper=formula(FullModel)),direction="forward")
formula(forwards)
#from the AIC values, the best model corresponds to trip_miles + Speed + pickup_community_area + dropoff_community_area + taxi_ID

#remove variables which are not required
rm(FullModel)
rm(EmptyModel)
rm(forwards)

#----------------------------Models------------------
#Baseline model - Random guessing
best.guess <- mean(taxi_Clean.train$trip_seconds)
RMSE.baseline <- rmse(taxi_Clean.test$trip_seconds,best.guess)
RMSLE.baseline <- rmsle(taxi_Clean.test$trip_seconds,best.guess)

#Linear model using the above variables
fit=lm(trip_seconds ~ trip_miles + Speed + pickup_community_area + dropoff_community_area + taxi_ID,data=taxi_Clean.train)
vif(fit)
summary(fit)
#shows moderate collinearity

predict.fit=predict(fit, taxi_Clean.test)
#irrelevant predictions that is time<=0 are set to 0
predict.fit= ifelse (predict.fit < 0, 0, predict.fit)
RMSE.lin=rmse(taxi_Clean.test$trip_seconds,predict.fit)
RMSLE.lin=rmsle(taxi_Clean.test$trip_seconds,predict.fit)

#Lasso and ridge
pows = seq(-5, 2.5, 0.1)
lambdas = 10^pows
length.lambdas = length(lambdas)
#getting a matrix and vector from training date for ridge regression
x = model.matrix(trip_seconds ~ ., data = taxi_Clean.train)
y = taxi_Clean.train$trip_seconds
x.test = model.matrix(trip_seconds ~ ., data = taxi_Clean.test)
ridge.mod=glmnet(x,y,alpha=0,lambda=lambdas)
#cross validation to pick the best Lambda
cv.out <- cv.glmnet(x, y, alpha=0, lambda=lambdas)
plot(cv.out)
best.lambda <- cv.out$lambda.min
#best lambda
best.lambda
ridge.predict = predict(ridge.mod, s = best.lambda, newx = x.test)
ridge.predict=ifelse(ridge.predict<0,0,ridge.predict)
#errors
RMSE.ridge=rmse(taxi_Clean.test$trip_seconds,ridge.predict)
RMSLE.ridge=rmsle(taxi_Clean.test$trip_seconds,ridge.predict)

#Lasso
lasso.fit = glmnet(x, y, alpha = 1)
#cross validation to get the best lambda
cv.out <- cv.glmnet(x, y)
plot(cv.out)
best.lambda <- cv.out$lambda.min
best.lambda
#using the best lambda to predict the time
lasso.pred = predict(lasso.fit, s = best.lambda, newx = x.test)
lasso.pred=ifelse(lasso.pred<0,0,lasso.pred)
#errors
RMSE.lasso=rmse(taxi_Clean.test$trip_seconds,lasso.pred)
RMSLE.lasso=rmsle(taxi_Clean.test$trip_seconds,lasso.pred)



#Random Forest
RF.taxi=randomForest(trip_seconds ~ ., data = taxi_Clean.train, mtry = 1, importance = T,ntree=100,na.action = na.omit)
bag.pred = predict(RF.taxi, taxi_Clean.test)
#irrelevant predictions that is time<=0 are set to 0
bag.pred = ifelse(bag.pred<0,0,bag.pred)
#errors
RMSE.rf=rmse(taxi_Clean.test$trip_seconds,bag.pred)
RMSLE.rf=rmsle(taxi_Clean.test$trip_seconds,bag.pred)
#variable importance
importance(RF.taxi)
varImpPlot(RF.taxi)


#boosting
test.errb1 = rep(NA, length.lambdas)
test.errb2 = rep(NA, length.lambdas)
#for multiple lambdas
for(i in 1:length.lambdas)
{
  gbm.taxi=gbm(trip_seconds ~ ., data = taxi_Clean.train,distribution="gaussian",n.trees=100, interaction.depth=3,shrinkage=lambdas[i])
  boost.pred = predict(gbm.taxi, taxi_Clean.test, n.trees=100)
  boost.pred = ifelse(boost.pred<0,0,boost.pred)
  test.errb1[i] = rmse(taxi_Clean.test$trip_seconds,boost.pred)
  test.errb2[i] = rmsle(taxi_Clean.test$trip_seconds,boost.pred)
}
#plot the errors
plot(lambdas, test.errb1, type = "b", xlab = "Shrinkage", ylab = "Test RMSE", pch = 20)
RMSE.boost=min(test.errb1)
plot(lambdas, test.errb2, type = "b", xlab = "Shrinkage", ylab = "Test RMSLE", pch = 20)
RMSLE.boost=min(test.errb2)

errors<-data.frame(c("Baseline", "Mulitpl-linear", "LASSO", "Ridge", "Random Forest", "Boosting"),c(RMSE.baseline,RMSE.lin,RMSE.lasso,RMSE.ridge,RMSE.rf,RMSE.boost),c(RMSLE.baseline,RMSLE.lin,RMSLE.lasso,RMSLE.ridge,RMSLE.rf,RMSLE.boost))
colnames(errors)<- c("Model","RMSE","RMSLE")
errors

