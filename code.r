##Converting price to 1-5 using quartiles



##Text Cleaning
install.packages("tm")
library(tm)
data$content<-Corpus(VectorSource(data$content))
data$content<-tm_map(data$content, tolower)
data$content<-tm_map(data$content, removeNumbers)
data$content<-tm_map(data$content, removeWords,stopwords())
data$content<-tm_map(data$content, removePunctuation)
data$content<-tm_map(data$content, stripWhitespace)

##Linear regression begins
##Reading file
final.edit.sentiment.with.price <- read.csv("G:/Master Studies/INST737/final edit sentiment with price.csv")
#Dividing data into 75-25% format
testidx<-which(1:nrow(final.edit.sentiment.with.price)%%4==0)
train_trip<-final.edit.sentiment.with.price[-testidx,]
test_trip<-final.edit.sentiment.with.price[testidx,]
attach(train_trip)

#linear regression of all individual varibles in first iteration of iteration 1
#regression using service ratings
fit_service.lm<-lm(Overall~Service) #linear regression
pre_service.fit<-predict(fit_service.lm,newdata = test_trip) #predicting new dependent variable values on test data
summary(fit_service.lm) #summarizing the linear model
cor(pre_service.fit,test_trip$Overall)# computing correlations
mse(test_trip$Overall,pre_service.fit)# computing mean square error
pc1_service<-predict(fit_service.lm,int="c",newdata=test_trip) 
pp1_service<-predict(fit_service.lm,int="p",newdata=test_trip)
plot(Service,Overall)
matlines(pre_service.fit,pc1_service) #plotting confidence band
matlines(pre_service.fit,pp1_service) #plotting prediction band
qqnorm(residuals(fit_service.lm)) #plotting residual values
qqline(residuals(fit_service.lm)) #plotting normal line

#regression using value ratings
fit_value.lm<-lm(Overall~Value) 
pre_value<-predict(fit_value.lm,newdata = test_trip)
summary(fit_value.lm)
cor(pre_value,test_trip$Overall)
mse(test_trip$Overall,pre_value)
pc1_value<-predict(fit_value.lm,int="c",newdata=test_trip)
pp1_value<-predict(fit_value.lm,int="p",newdata=test_trip)
plot(Value,Overall)
matlines(pre_value,pc1_value)
matlines(pre_value,pp1_value)
qqnorm(residuals(fit_value.lm))
qqline(residuals(fit_value.lm))

#regression using room ratings
fit_rooms.lm<-lm(Overall~Rooms)
pre_rooms<-predict(fit_rooms.lm,newdata = test_trip)
pc1_rooms<-predict(fit_rooms.lm,int="c",newdata=test_trip)
pp1_rooms<-predict(fit_rooms.lm,int="p",newdata=test_trip)
plot(Rooms,Overall)
matlines(pre_rooms,pc1_rooms)
matlines(pre_rooms,pp1_rooms)
cor(pre_rooms,test_trip$Overall)
mse(test_trip$Overall,pre_rooms)
summary(fit_rooms.lm)
qqnorm(residuals(fit_rooms.lm))
qqline(residuals(fit_rooms.lm))

#regression using ratings of location of hotel
fit_location.lm<-lm(Overall~Location)
pre_location<-predict(fit_location.lm,newdata = test_trip)
pc1_location<-predict(fit_location.lm,int="c",newdata=test_trip)
pp1_location<-predict(fit_location.lm,int="p",newdata=test_trip)
plot(Location,Overall)
matlines(pre_location,pc1_location)
matlines(pre_location,pp1_location)
cor(pre_location,test_trip$Overall)
mse(test_trip$Overall,pre_location)
summary(fit_location.lm)

#regression using price of rooms of hotel
fit_price.lm<-lm(Overall~price)
pre_price<-predict(fit_price.lm,newdata = test_trip)
pc1_price<-predict(fit_price.lm,int="c",newdata=test_trip)
pp1_price<-predict(fit_price.lm,int="p",newdata=test_trip)
plot(price,Overall)
matlines(pre_price,pc1_price)
matlines(pre_price,pp1_price)
cor(pre_price,test_trip$Overall)
mse(test_trip$Overall,pre_price)
summary(fit_price.lm)

#regression using sentiment of reviews of customers
fit_sentiment.lm<-lm(Overall~Sentiment)
pre_sentiment<-predict(fit_sentiment.lm,newdata = test_trip)
pc1_sentiment<-predict(fit_sentiment.lm,int="c",newdata=test_trip)
pp1_sentiment<-predict(fit_sentiment.lm,int="p",newdata=test_trip)
plot(Sentiment,Overall)
matlines(pre_sentiment,pc1_sentiment)
matlines(pre_sentiment,pp1_sentiment)
cor(pre_sentiment,test_trip$Overall)
mse(test_trip$Overall,pre_sentiment)
summary(fit_sentiment.lm)


# models of multivariate regression 
# model with all independent variables
fit_all.lm<-lm(Overall~Service+Value+Rooms+Location+price+Sentiment, data = train_trip)
pre_all<-predict(fit_all.lm,newdata = test_trip)
summary(fit_all.lm)
cor(pre_all,test_trip$Overall)
mse(test_trip$Overall,pre_all)
qqnorm(residuals(fit_all.lm))
qqline(residuals(fit_all.lm))
#code block for regularization
#adding all columns other than dependent variable using cv.fit (glmnet package) for regularization
cv1.fit<-cv.glmnet(as.matrix(train_trip[-1,c(-1,-3,-6)]),as.vector(train_trip[-1,6]),alpha=1)
plot(cv1.fit)
coef(cv1.fit)#computing the coefficients and intercept value
cv.fit$lambda.min #minimum value of lambda
prediction1<-predict(cv1.fit,newx = as.matrix(test_trip[,c(-1,-3,-6)])) #predicting regularized value of dependent variable
cor(prediction1,as.vector(test_trip[,6])) #correlation between predicted and actual value

#model using Service+Value
fit_model1.lm<-lm(Overall~Service+Value, data = train_trip)
pre_model1<-predict(fit_model1.lm,newdata = test_trip)
summary(fit_model1.lm)
cor(pre_model1,test_trip$Overall)
mse(test_trip$Overall,pre_model1)
anova(fit_service.lm,fit_model1.lm)
qqnorm(residuals(fit_model1.lm))
qqline(residuals(fit_model1.lm))
cv11.fit<-cv.glmnet(as.matrix(train_trip[-1,c(-1,-3,-5,-6,-7,-8,-9)]),as.vector(train_trip[-1,6]),alpha=1)
plot(cv11.fit)
coef(cv11.fit)
cv11.fit$lambda.min
prediction11<-predict(cv11.fit,newx = as.matrix(test_trip[,c(-1,-3,-5,-6,-7,-8,-9)]))
cor(prediction11,as.vector(test_trip[,6]))

#model using Service+Value+Rooms
fit_model2.lm<-lm(Overall~Service+Value+Rooms, data = train_trip)
pre_model2<-predict(fit_model2.lm,newdata = test_trip)
summary(fit_model2.lm)
cor(pre_model2,test_trip$Overall)
mse(test_trip$Overall,pre_model2)
anova(fit_model1.lm,fit_model2.lm)
qqnorm(residuals(fit_model2.lm))
qqline(residuals(fit_model2.lm))
cv12.fit<-cv.glmnet(as.matrix(train_trip[-1,c(-1,-3,-6,-7,-8,-9)]),as.vector(train_trip[-1,6]),alpha=1)
plot(cv12.fit)
coef(cv12.fit)
cv12.fit$lambda.min
prediction12<-predict(cv12.fit,newx = as.matrix(test_trip[,c(-1,-3,-6,-7,-8,-9)]))
cor(prediction12,as.vector(test_trip[,6]))

#model using Service+Value+Rooms+Location
fit_model3.lm<-lm(Overall~Service+Value+Rooms+Location, data = train_trip)
pre_model3<-predict(fit2_model3.lm,newdata = test_trip)
summary(fit_model3.lm)
cor(pre_model3,test_trip$Overall)
mse(test_trip$Overall,pre_model3)
anova(fit_model2.lm,fit_model3.lm)
qqnorm(residuals(fit_model3.lm))
qqline(residuals(fit_model3.lm))
cv13.fit<-cv.glmnet(as.matrix(train_trip[-1,c(-1,-3,-6,-7,-8,-9)]),as.vector(train_trip[-1,6]),alpha=1)
plot(cv13.fit)
coef(cv13.fit)
cv13.fit$lambda.min
prediction13<-predict(cv13.fit,newx = as.matrix(test_trip[,c(-1,-3,-6,-7,-8,-9)]))
cor(prediction13,as.vector(test_trip[,6]))

#model using Service+Value+Rooms+Location+price
fit_model4.lm<-lm(Overall~Service+Value+Rooms+Location+price, data = train_trip)
pre_model4<-predict(fit2_model4.lm,newdata = test_trip)
summary(fit_model4.lm)
cor(pre_model4,test_trip$Overall)
mse(test_trip$Overall,pre_model4)
anova(fit_model3.lm,fit_model4.lm)
qqnorm(residuals(fit_model4.lm))
qqline(residuals(fit_model4.lm))


#model using Service+Value+Rooms+Location+Sentiment
fit_model5.lm<-lm(Overall~Service+Value+Rooms+Location+Sentiment, data = train_trip)
pre_model5<-predict(fit_model5.lm,newdata = test_trip)
summary(fit_model5.lm)
cor(pre_model5,test_trip$Overall)
mse(test_trip$Overall,pre_model5)
anova(fit_all.lm,fit_model5.lm)
qqnorm(residuals(fit_model5.lm))
qqline(residuals(fit_model5.lm))
cv15.fit<-cv.glmnet(as.matrix(train_trip[-1,c(-1,-3,-6,-8)]),as.vector(train_trip[-1,6]),alpha=1)
plot(cv15.fit)
coef(cv15.fit)
cv15.fit$lambda.min
prediction15<-predict(cv15.fit,newx = as.matrix(test_trip[,c(-1,-3,-6,-8)]))
cor(prediction12,as.vector(test_trip[,6]))


#second iteration with 80-20 data
#Dividing data into 80:20 train and test ratio
new_test1<-which(1:nrow(final.edit.sentiment.with.price)%%5==0)
train<-final.edit.sentiment.with.price[-new_test1,]
test<-final.edit.sentiment.with.price[new_test1,]
nrow(train)
nrow(test)
train1_trip<-train
test1_trip<-test

#regression using service ratings
fit1_service.lm<-lm(Overall~Service, data = train1_trip)
pre1_service.fit<-predict(fit1_service.lm,newdata = test1_trip)
matlines(pre_sentiment,pc1_sentiment)
matlines(pre_sentiment,pp1_sentiment)
summary(fit1_service.lm)
cor(pre1_service.fit,test1_trip$Overall)
mse(test1_trip$Overall,pre1_service.fit)
pc2_service<-predict(fit1_service.lm,int="c",newdata=test1_trip)
pp2_service<-predict(fit1_service.lm,int="p",newdata=test1_trip)
plot(Service,Overall)
matlines(pre1_service.fit,pc2_service)
matlines(pre1_service.fit,pp2_service)
qqnorm(residuals(fit1_service.lm))
qqline(residuals(fit1_service.lm))

#regression using value ratings
fit1_value.lm<-lm(Overall~Value)
pre1_value<-predict(fit1_value.lm,newdata = test1_trip)
summary(fit1_value.lm)
cor(pre1_value,test1_trip$Overall)
mse(test1_trip$Overall,pre1_value)
pc2_value<-predict(fit1_value.lm,int="c",newdata=test1_trip)
pp2_value<-predict(fit1_value.lm,int="p",newdata=test1_trip)
plot(Value,Overall)
matlines(pre1_value,pc2_value)
matlines(pre1_value,pp2_value)
qqnorm(residuals(fit1_value.lm))
qqline(residuals(fit1_value.lm))

#regression using rooms ratings
fit1_rooms.lm<-lm(Overall~Rooms)
pre1_rooms<-predict(fit1_rooms.lm,newdata = test1_trip)
summary(fit1_rooms.lm)
cor(pre1_rooms,test1_trip$Overall)
mse(test1_trip$Overall,pre1_rooms)
pc2_rooms<-predict(fit1_rooms.lm,int="c",newdata=test1_trip)
pp2_rooms<-predict(fit1_rooms.lm,int="p",newdata=test1_trip)
plot(Rooms,Overall)
matlines(pre1_rooms,pc2_rooms)
matlines(pre1_rooms,pp2_rooms)
qqnorm(residuals(fit1_rooms.lm))
qqline(residuals(fit1_rooms.lm))

#regression using location ratings
fit1_location.lm<-lm(Overall~Location)
pre1location<-predict(fit1_location.lm,newdata = test1_trip)
summary(fit1_location.lm)
cor(pre1_location,test1_trip$Overall)
mse(test1_trip$Overall,pre1_location)
pc2_location<-predict(fit1_location.lm,int="c",newdata=test1_trip)
pp2_location<-predict(fit1_location.lm,int="p",newdata=test1_trip)
plot(Location,Overall)
matlines(pre1_location,pc2_location)
matlines(pre1_location,pp2_location)
qqnorm(residuals(fit1_location.lm))
qqline(residuals(fit1_location.lm))

#regression using price of hotel rooms
fit1_price.lm<-lm(Overall~price)
pre1_price<-predict(fit1_price.lm,newdata = test1_trip)
summary(fit1_price.lm)
cor(pre1_price,test1_trip$Overall)
mse(test1_trip$Overall,pre1_price)
pc2_price<-predict(fit1_price.lm,int="c",newdata=test1_trip)
pp2_price<-predict(fit1_price.lm,int="p",newdata=test1_trip)
plot(price,Overall)
matlines(pre1_price,pc2_price)
matlines(pre1_price,pp2_price)
qqnorm(residuals(fit1_price.lm))
qqline(residuals(fit1_price.lm))

#regression using sentiment of customer reviews
fit1_sentiment.lm<-lm(Overall~Sentiment, data=train1_trip)
pre2_sentiment<-predict(fit1_sentiment.lm,newdata = test1_trip)
summary(fit1_sentiment.lm)
cor(pre1_sentiment,test1_trip$Overall)
mse(test1_trip$Overall,pre1_sentiment)
pc2_sentiment<-predict(fit1_sentiment.lm,int="c",newdata=test1_trip)
pp2_sentiment<-predict(fit1_sentiment.lm,int="p",newdata=test1_trip)
plot(Sentiment,Overall)
matlines(pre1_sentiment,pc2_sentiment)
matlines(pre1_sentiment,pp2_sentiment)
qqnorm(residuals(fit1_sentiment.lm))
qqline(residuals(fit1_sentiment.lm))

#multivariate regression
#regression using all independent variables to predict dependent variable
fit1_all.lm<-lm(Overall~Service+Value+Rooms+Location+price+Sentiment, data = train1_trip)
pre1_all<-predict(fit1_all.lm,newdata = test1_trip)
summary(fit1_all.lm)
cor(pre1_all,test1_trip$Overall)
mse(test1_trip$Overall,pre1_all)
qqnorm(residuals(fit1_all.lm))
qqline(residuals(fit1_all.lm))
cv2.fit<-cv.glmnet(as.matrix(train1_trip[-1,c(-1,-3,-6,-8)]),as.vector(train1_trip[-1,6]),alpha=1)
plot(cv2.fit)
coef(cv2.fit)
cv2.fit$lambda.min
prediction2<-predict(cv2.fit,newx = as.matrix(test1_trip[,c(-1,-3,-6,-8)]))
cor(prediction2,as.vector(test1_trip[,6]))

#regression using Service+Value
fit1_model1.lm<-lm(Overall~Service+Value, data = train1_trip)
pre1_model1<-predict(fit1_model1.lm,newdata = test1_trip)
summary(fit1_model1.lm)
cor(pre1_model1,test1_trip$Overall)
mse(test1_trip$Overall,pre1_model1)
anova(fit1_service.lm,fit1_model1.lm)
qqnorm(residuals(fit1_model1.lm))
qqline(residuals(fit1_model1.lm))
cv21.fit<-cv.glmnet(as.matrix(train1_trip[-1,c(-1,-3,-5,-6,-7,-8,-9)]),as.vector(train1_trip[-1,6]),alpha=1)
plot(cv21.fit)
coef(cv21.fit)
cv21.fit$lambda.min
prediction21<-predict(cv21.fit,newx = as.matrix(test1_trip[,c(-1,-3,-5,-6,-7,-8,-9)]))
cor(prediction21,as.vector(test1_trip[,6]))

#regression using Service+Value+Rooms
fit1_model2.lm<-lm(Overall~Service+Value+Rooms, data = train1_trip)
pre1_model2<-predict(fit1_model2.lm,newdata = test1_trip)
summary(fit1_model2.lm)
cor(pre1_model2,test1_trip$Overall)
mse(test1_trip$Overall,pre1_model2)
anova(fit1_model1.lm,fit1_model2.lm)
qqnorm(residuals(fit1_model2.lm))
qqline(residuals(fit1_model2.lm))
cv22.fit<-cv.glmnet(as.matrix(train1_trip[-1,c(-1,-3,-6,-7,-8,-9)]),as.vector(train1_trip[-1,6]),alpha=1)
plot(cv22.fit)
coef(cv22.fit)
cv22.fit$lambda.min
prediction22<-predict(cv22.fit,newx = as.matrix(test1_trip[,c(-1,-3,-6,-7,-8,-9)]))
cor(prediction22,as.vector(test1_trip[,6]))

#regression using Service+Value+Rooms+Location
fit1_model3.lm<-lm(Overall~Service+Value+Rooms+Location, data = train1_trip)
pre1_model3<-predict(fit1_model3.lm,newdata = test1_trip)
summary(fit1_model3.lm)
cor(pre1_model3,test1_trip$Overall)
mse(test1_trip$Overall,pre1_model3)
anova(fit1_model2.lm,fit1_model3.lm)
qqnorm(residuals(fit1_model3.lm))
qqline(residuals(fit1_model3.lm))
cv23.fit<-cv.glmnet(as.matrix(train1_trip[-1,c(-1,-3,-6,-8,-9)]),as.vector(train1_trip[-1,6]),alpha=1)
plot(cv23.fit)
coef(cv23.fit)
cv23.fit$lambda.min
prediction23<-predict(cv23.fit,newx = as.matrix(test1_trip[,c(-1,-3,-6,-8,-9)]))
cor(prediction23,as.vector(test1_trip[,6]))

#regression using Service+Value+Rooms+Location+price
fit1_model4.lm<-lm(Overall~Service+Value+Rooms+Location+price, data = train1_trip)
pre1_model4<-predict(fit1_model4.lm,newdata = test1_trip)
summary(fit1_model4.lm)
cor(pre1_model4,test1_trip$Overall)
mse(test1_trip$Overall,pre1_model4)
anova(fit1_model3.lm,fit1_model4.lm)
qqnorm(residuals(fit1_model4.lm))
qqline(residuals(fit1_model4.lm))

#regression using Service+Value+Rooms+Location+Sentiment
fit1_model5.lm<-lm(Overall~Service+Value+Rooms+Location+Sentiment, data = train1_trip)
pre1_model5<-predict(fit1_model5.lm,newdata = test1_trip)
summary(fit1_model5.lm)
cor(pre1_model5,test1_trip$Overall)
mse(test1_trip$Overall,pre1_model5)
anova(fit1_model4.lm,fit1_model5.lm)
qqnorm(residuals(fit1_model5.lm))
qqline(residuals(fit1_model5.lm))
cv25.fit<-cv.glmnet(as.matrix(train1_trip[-1,c(-1,-3,-6,-8)]),as.vector(train1_trip[-1,6]),alpha=1)
plot(cv25.fit)
coef(cv25.fit)
cv25.fit$lambda.min
prediction25<-predict(cv25.fit,newx = as.matrix(test1_trip[,c(-1,-3,-6,-8)]))
cor(prediction25,as.vector(test1_trip[,6]))

#third iteration with 66-34 division 
#dividing the dataset into train and test data
new_test2<-which(1:nrow(final.edit.sentiment.with.price)%%3==0)
train2_trip<-final.edit.sentiment.with.price[-new_test2,]
test2_trip<-final.edit.sentiment.with.price[new_test2,]

#regression using service ratings
fit2_service.lm<-lm(Overall~Service, data = train2_trip)
pre2_service.fit<-predict(fit2_service.lm,newdata = test2_trip)
summary(fit2_service.lm)
cor(pre2_service.fit,test2_trip$Overall)
mse(test2_trip$Overall,pre2_service.fit)
pc3_service<-predict(fit2_service.lm,int="c",newdata=test2_trip)
pp3_service<-predict(fit2_service.lm,int="p",newdata=test2_trip)
plot(Service,Overall)
matlines(pre2_service.fit,pc3_service)
matlines(pre2_service.fit,pp3_service)
qqnorm(residuals(fit2_service.lm))
qqline(residuals(fit2_service.lm))

#regression using value ratings
fit2_value.lm<-lm(Overall~Value)
pre2_value<-predict(fit2_value.lm,newdata = test2_trip)
summary(fit2_value.lm)
cor(pre2_value,test2_trip$Overall)
mse(test2_trip$Overall,pre2_value)
pc3_value<-predict(fit2_value.lm,int="c",newdata=test2_trip)
pp3_value<-predict(fit2_value.lm,int="p",newdata=test2_trip)
plot(Value,Overall)
matlines(pre2_value,pc3_value)
matlines(pre2_value,pp3_value)
qqnorm(residuals(fit2_value.lm))
qqline(residuals(fit2_value.lm))

#regression using rooms ratings
fit2_rooms.lm<-lm(Overall~Rooms)
pre2_rooms<-predict(fit2_rooms.lm,newdata = test2_trip)
summary(fit2_rooms.lm)
cor(pre2_rooms,test2_trip$Overall)
mse(test2_trip$Overall,pre2_rooms)
pc3_rooms<-predict(fit2_rooms.lm,int="c",newdata=test2_trip)
pp3_rooms<-predict(fit2_rooms.lm,int="p",newdata=test2_trip)
plot(Rooms,Overall)
matlines(pre2_rooms,pc3_rooms)
matlines(pre2_rooms,pp3_rooms)
qqnorm(residuals(fit2_rooms.lm))
qqline(residuals(fit2_rooms.lm))

#regression using Hotel's Location ratings
fit2_location.lm<-lm(Overall~Location)
pre2_location<-predict(fit2_location.lm,newdata = test2_trip)
summary(fit2_location.lm)
cor(pre2_location,test2_trip$Overall)
mse(test2_trip$Overall,pre2_location)
pc3_location<-predict(fit2_location.lm,int="c",newdata=test2_trip)
pp3_location<-predict(fit2_location.lm,int="p",newdata=test2_trip)
plot(Location,Overall)
matlines(pre2_location,pc3_location)
matlines(pre2_location,pp3_location)
qqnorm(residuals(fit2_location.lm))
qqline(residuals(fit2_location.lm))

#regression using price off hotel rooms
fit2_price.lm<-lm(Overall~price)
pre2_price<-predict(fit2_price.lm,newdata = test2_trip)
summary(fit2_price.lm)
cor(pre2_price,test2_trip$Overall)
mse(test2_trip$Overall,pre2_price)
pc3_price<-predict(fit2_price.lm,int="c",newdata=test2_trip)
pp3_price<-predict(fit2_price.lm,int="p",newdata=test2_trip)
plot(price,Overall)
matlines(pre2_price,pc3_price)
matlines(pre2_price,pp3_price)
qqnorm(residuals(fit2_price.lm))
qqline(residuals(fit2_price.lm))

#regression using sentiment of customer reviews
fit2_sentiment.lm<-lm(Overall~Sentiment, data=train2_trip)
pre2_sentiment<-predict(fit2_sentiment.lm,newdata = test2_trip)
summary(fit2_sentiment.lm)
cor(pre2_sentiment,test2_trip$Overall)
mse(test2_trip$Overall,pre2_sentiment)
pc3_sentiment<-predict(fit2_sentiment.lm,int="c",newdata=test2_trip)
pp3_sentiment<-predict(fit2_sentiment.lm,int="p",newdata=test2_trip)
plot(Sentiment,Overall)
matlines(pre2_sentiment,pc3_sentiment)
matlines(pre2_sentiment,pp3_sentiment)
qqnorm(residuals(fit2_sentiment.lm))
qqline(residuals(fit2_sentiment.lm))

#Multivaraite regression
#regression using all independent varibles
fit2_all.lm<-lm(Overall~Service+Value+Rooms+Location+price+Sentiment, data = train2_trip)
pre2_all<-predict(fit2_all.lm,newdata = test2_trip)
summary(fit2_all.lm)
cor(pre2_all,test2_trip$Overall)
mse(test2_trip$Overall,pre2_all)
qqnorm(residuals(fit2_all.lm))
qqline(residuals(fit2_all.lm))
cv3.fit<-cv.glmnet(as.matrix(train2_trip[-1,c(-1,-3,-6,-8)]),as.vector(train2_trip[-1,6]),alpha=1)
plot(cv3.fit)
coef(cv3.fit)
cv3.fit$lambda.min
prediction3<-predict(cv3.fit,newx = as.matrix(test2_trip[,c(-1,-3,-6,-8)]))
cor(prediction3,as.vector(test2_trip[,6]))

#regression using Service+Value
fit2_model1.lm<-lm(Overall~Service+Value, data = train2_trip)
pre2_model1<-predict(fit2_model1.lm,newdata = test2_trip)
summary(fit2_model1.lm)
cor(pre2_model1,test2_trip$Overall)
mse(test2_trip$Overall,pre2_model1)
anova(fit2_service.lm,fit2_model1.lm)
qqnorm(residuals(fit2_model1.lm))
qqline(residuals(fit2_model1.lm))
cv31.fit<-cv.glmnet(as.matrix(train2_trip[-1,c(-1,-3,-5,-6,-7,-8,-9)]),as.vector(train2_trip[-1,6]),alpha=1)
plot(cv31.fit)
coef(cv31.fit)
cv31.fit$lambda.min
prediction31<-predict(cv31.fit,newx = as.matrix(test2_trip[,c(-1,-3,-5,-6,-7,-8,-9)]))
cor(prediction31,as.vector(test2_trip[,6]))

#regression using Service+Value+Rooms
fit2_model2.lm<-lm(Overall~Service+Value+Rooms, data = train2_trip)
pre2_model2<-predict(fit2_model2.lm,newdata = test2_trip)
summary(fit2_model2.lm)
cor(pre2_model2,test2_trip$Overall)
mse(test2_trip$Overall,pre2_model2)
anova(fit2_model1.lm,fit2_model2.lm)
qqnorm(residuals(fit2_model2.lm))
qqline(residuals(fit2_model2.lm))
cv32.fit<-cv.glmnet(as.matrix(train2_trip[-1,c(-1,-3,-6,-7,-8,-9)]),as.vector(train2_trip[-1,6]),alpha=1)
plot(cv32.fit)
coef(cv32.fit)
cv32.fit$lambda.min
prediction32<-predict(cv32.fit,newx = as.matrix(test2_trip[,c(-1,-3,-6,-7,-8,-9)]))
cor(prediction32,as.vector(test2_trip[,6]))

#regression using Service+Value+Rooms+Location
fit2_model3.lm<-lm(Overall~Service+Value+Rooms+Location, data = train2_trip)
pre2_model3<-predict(fit2_model3.lm,newdata = test2_trip)
summary(fit2_model3.lm)
cor(pre2_model3,test2_trip$Overall)
mse(test2_trip$Overall,pre2_model3)
anova(fit2_model1.lm,fit2_model3.lm)
qqnorm(residuals(fit2_model3.lm))
qqline(residuals(fit2_model3.lm))
cv33.fit<-cv.glmnet(as.matrix(train2_trip[-1,c(-1,-3,-6,-8,-9)]),as.vector(train2_trip[-1,6]),alpha=1)
plot(cv33.fit)
coef(cv33.fit)
cv33.fit$lambda.min
prediction33<-predict(cv33.fit,newx = as.matrix(test2_trip[,c(-1,-3,-6,-8,-9)]))
cor(prediction33,as.vector(test2_trip[,6]))

#regression using Service+Value+Rooms+Location+price
fit2_model4.lm<-lm(Overall~Service+Value+Rooms+Location+price, data = train2_trip)
pre2_model4<-predict(fit2_model4.lm,newdata = test2_trip)
summary(fit2_model4.lm)
cor(pre2_model4,test2_trip$Overall)
mse(test2_trip$Overall,pre2_model4)
anova(fit2_model1.lm,fit2_model4.lm)
qqnorm(residuals(fit2_model4.lm))
qqline(residuals(fit2_model4.lm))

#regression using Service+Value+Rooms+Location+Sentiment
fit2_model5.lm<-lm(Overall~Service+Value+Rooms+Location+Sentiment, data = train2_trip)
pre2_model5<-predict(fit2_model5.lm,newdata = test2_trip)
summary(fit2_model5.lm)
cor(pre2_model5,test2_trip$Overall)
mse(test2_trip$Overall,pre2_model5)
anova(fit2_model1.lm,fit2_model5.lm)
qqnorm(residuals(fit2_model5.lm))
qqline(residuals(fit2_model5.lm))
cv35.fit<-cv.glmnet(as.matrix(train2_trip[-1,c(-1,-3,-6,-8)]),as.vector(train2_trip[-1,6]),alpha=1)
plot(cv35.fit)
coef(cv35.fit)
cv35.fit$lambda.min
prediction35<-predict(cv35.fit,newx = as.matrix(test2_trip[,c(-1,-3,-6,-8)]))
cor(prediction35,as.vector(test2_trip[,6]))


#comparing two models to see which is better depending upon p value
anova(fit1_model4.lm,fit1_model3.lm)

 
#Plotting various spineplots(used in presentation)
Spine_service<-factor(Service)
Spine_Overall<-factor(Overall)
Spine_value<-factor(Value)
Spine_rooms<-factor(Rooms)
Spine_location<-factor(Location)
spineplot(Spine_service,Spine_Overall,xlab = "Service",ylab = "Overall", colors(TRUE))
spineplot(Spine_value,Spine_Overall,xlab="Value",ylab="Overall")
spineplot(Spine_location,Spine_Overall,xlab = "Location",ylab="Overall")
spineplot(Spine_rooms,Spine_Overall,xlab="Rooms",ylab="Overall")

##Linear Regression ends

##logistic regression begins
##reading dataset.csv to carry logistic expression and naives bayes classification
final.edit.sentiment.with.price <- read.csv("C:/Users/sanjn/Desktop/datafinal.csv",  stringsAsFactors = FALSE, na.strings = c(NA,NULL,""))
View(final.edit.sentiment.with.price)

##creating and viewing test and train data

testidx<-which(1:nrow(final.edit.sentiment.with.price)%%4==0)
train_trip<-final.edit.sentiment.with.price[-testidx,]
test_trip<-final.edit.sentiment.with.price[testidx,]
View(train_trip)
summary(train_trip)
View(test_trip)
summary(test_trip)

#trying to perform multinomial regression
train_trip$Overall=factor(train_trip$Overall)
model<-multinom(train_trip$Overall ~ train_trip$Service + train_trip$Value + train_trip$Rooms + train_trip$Location + train_trip$price + train_trip$Sentiment,data = train_trip)
summary(model)
predict(model,test_trip)

##finding p value
z<- summary(model)$coefficients/summary(model)$standard.error
z
p <- (1 - pnorm(abs(z), 0, 1))*2
p

#confusion matrix
Confusionmatrix <-table(predict(model), train_trip$Overall)
print(Confusionmatrix)

##stopped due to bad results


##logistic model
##obtaining the intercept, co-efficients of each feature and log-odds
library(glmnet)
train_trip$Overall=factor(train_trip$Overall)

##DV with individual IV's

mlogit<-glm(Overall~Service + Value + Rooms + Location + Sentiment + price, data=train_trip,family="binomial",maxit=100)
summary(mlogit)

##coefficients of the logistic regression model- log odds
coef(mlogit)

##odd ratios of the outcome
exp(coef(mlogit))

##predicting eliteStatus using test data
test_trip$Overall<- predict(mlogit,test_trip,type="response")


##finding correlation between the predicted and actual value for eliteStatus
cor(train_trip$Overall,test_trip$Overall)

##regularization to find best predictors
x.reg <- as.matrix(train_trip[,c("price","Location","Service","Rooms","Value","Sentiment")])
y.reg <- as.vector(train_trip[,"Overall"])
cv.logreg.model <- cv.glmnet(x = x.reg,y = y.reg,alpha=1)
summary(train_trip)
str(train_trip)
summary(cv.logreg.model)

##coefficients of the logistic regression model
coef(cv.logreg.model)

##odd ratios of the outcome
exp(coef(cv.logreg.model))

##checking prediction and correlation after regularization
test_trip$Overall_new<- predict(cv.logreg.model,newdata=test_trip$Overall,type="response")

##finding correlation between the predicted and actual value for eliteStatus after regularization
cor(test_trip$Overall_new,test_trip$Overall)

##Naive Bayes begins

setwd("C:/Users/Hardy/Desktop/inst737")
hotel_bayes <- read.csv("finaleditsentimentwithprice.csv") hotel_bayes$Cleanliness <- NULL

##categorizing the dependent variable
hotel_bayes$rating[hotel_bayes$Overall < 3] <- 0
hotel_bayes$rating[hotel_bayes$Overall >= 3] <- 1
View(hotel_bayes)
hotel_bayes$Overall <- NULL

##converting it as factor
hotel_bayes$rating <- as.factor(hotel_bayes$rating)

##splitting data into 75-25 training and testing data
row_divider = nrow(hotel_bayes)*0.75
View(hotel_bayes)
train_data_bayes=hotel_bayes[1:row_divider,]
test_data_bayes=hotel_bayes[row_divider:nrow(hotel_bayes),]

##finding proportion of the two dataset
prop.table(table(train_data_bayes$rating))
prop.table(table(test_data_bayes$rating))
library(e1071)

##naïvebayes
rating_classifier <- naiveBayes(train_data_bayes,train_data_bayes$rating)
rating_classifier
rating_classifier_pred <- predict(rating_classifier,test_data_bayes)
summary(rating_classifier_pred)
library(gmodels)
CrossTable(rating_classifier_pred,test_data_bayes$rating,prop.chisq = F,prop.t = F,prop.r = F,dnn = c("predicted","actual"))


##naivebayes with laplace smoothing
rating_classifier_laplace <- naiveBayes(train_data_bayes, train_data_bayes$rating,laplace = 1)
rating_classifier_laplace
rating_classifier_pred_laplace <- predict(rating_classifier_laplace,test_data_bayes)
summary(rating_classifier_pred_laplace)
CrossTable(rating_classifier_pred_laplace,test_data_bayes$rating,prop.chisq = F,prop.t = F,prop.r = F,dnn = c("predicted","actual"))

##decision tree begins
##Dataset
View(hotel_DT)

##Randomly creating training and testing sets
set.seed(12345)
hotel_DT_rand	<-	hotel_DT[order(runif(57688)),	]
hotel_DT_train <- hotel_DT_rand[1:51920,]
hotel_DT_test <- hotel_DT_rand[51921:57688,]

##Checking proportion of dependent variable 'overall' in training and testing data
prop.table(table(hotel_DT_train$Overall))
prop.table(table(hotel_DT_test$Overall))

##converting dependent variable as factor
hotel_DT_train$Overall <- as.factor(hotel_DT_train$Overall)
hotel_DT_test$Overall <- as.factor(hotel_DT_test$Overall)


library(C50)  
hotel_DT_predict <- C5.0(hotel_DT_train[-4],	hotel_DT_train$Overall)
hotel_DT_predict
##hotel_new$price <- NULL
##hotel_DT_predict2 <- C5.0(hotel_new[-4,],	hotel_new$Overall)

summary(hotel_DT_predict)
hotel_pred	<-	predict(hotel_DT_predict,	hotel_DT_test)
CrossTable(hotel_DT_test$Overall,	hotel_pred, prop.chisq	=	FALSE,	prop.c	=	FALSE,	prop.r	= FALSE, dnn	=	c('actual	default',	'predicted	default'))

##boost
hotel_boost10	<-	C5.0(hotel_DT_train[-4],	hotel_DT_train$Overall, trials	=	10)
hotel_boost10
summary(hotel_boost10)
hotel_boost_pred10	<-	predict(hotel_boost10,	hotel_DT_test)
CrossTable(hotel_DT_test$Overall,	hotel_boost_pred10, prop.chisq	=	FALSE,	prop.c	=	FALSE,	prop.r	= FALSE, dnn	=	c('actual	default',	'predicted	default'))

##decision tree ends

##random forest begins

setwd("C:/Users/Hardy/Desktop/inst737")
hotel <- read.csv("finaleditsentimentwithprice.csv")
hotel$Cleanliness <- NULL
hotel$Overall <- as.factor(hotel$Overall)

#randomly splitting data
set.seed(123)
hotel_random<-hotel[order(runif(57688)),]
hotel_random_train<- hotel_random[1:46150,]
hotel_random_test<- hotel_random[46151:57688,]

##checking proportion after split
prop.table(table(hotel_random_train$Overall))

prop.table(table(hotel_random_test$Overall))

library(ISLR)	
library(MASS)	
library(randomForest)
library(gmodels)

##bagging with different tree numbers
bag.hotel	<-	randomForest(Overall~.,data=hotel_random_train,mtry=6,importance=TRUE)
bag.hotel
pred<-predict(bag.hotel, hotel_random_test)
CrossTable(hotel_random_test$Overall, pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c("Actual","Predicted"))
varImpPlot(bag.hotel)


bag.hotel	<-	randomForest(Overall~.,data=hotel_random_train,mtry=6,importance=TRUE, ntree= 100)
bag.hotel
CrossTable(hotel_random_test$Overall, pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c("Actual","Predicted"))
varImpPlot(bag.hotel)

bag.hotel	<-	randomForest(Overall~.,data=hotel_random_train,mtry=6,importance=TRUE, ntree= 25)
bag.hotel
CrossTable(hotel_random_test$Overall, pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c("Actual","Predicted"))
varImpPlot(bag.hotel)


bag.hotel	<-	randomForest(Overall~.,data=hotel_random_train,mtry=6,importance=TRUE, ntree= 250)
bag.hotel
CrossTable(hotel_random_test$Overall, pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c("Actual","Predicted"))
varImpPlot(bag.hotel)

bag.hotel	<-	randomForest(Overall~.,data=hotel_random_train,mtry=6,importance=TRUE, ntree= 750)
bag.hotel
CrossTable(hotel_random_test$Overall, pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c("Actual","Predicted"))
varImpPlot(bag.hotel)


bag.hotel	<-	randomForest(Overall~.,data=hotel_random_train,mtry=6,importance=TRUE, ntree= 1000)
bag.hotel
CrossTable(hotel_random_test$Overall, pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c("Actual","Predicted"))

##to find best feature
varImpPlot(bag.hotel)


##random forest with different tree numbers and m = 3 
forest.hotel <- randomForest(Overall~., data = hotel_random_train, mtry = 3, importance= TRUE, ntree = 250)
pred <-predict(forest.hotel, hotel_random_test)
CrossTable(hotel_random_test$Overall, pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c("Actual","Predicted"))
varImpPlot(forest.hotel)


forest.hotel <- randomForest(Overall~., data = hotel_random_train, mtry = 3, importance= TRUE, ntree = 50)
pred <-predict(forest.hotel, hotel_random_test)
CrossTable(hotel_random_test$Overall, pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c("Actual","Predicted"))
varImpPlot(forest.hotel)


forest.hotel <- randomForest(Overall~., data = hotel_random_train, mtry = 3, importance= TRUE, ntree = 100)
pred <-predict(forest.hotel, hotel_random_test)
CrossTable(hotel_random_test$Overall, pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c("Actual","Predicted"))
varImpPlot(forest.hotel)


forest.hotel <- randomForest(Overall~., data = hotel_random_train, mtry = 3, importance= TRUE, ntree = 500)
pred <-predict(forest.hotel, hotel_random_test)
CrossTable(hotel_random_test$Overall, pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c("Actual","Predicted"))

##to find best feature
varImpPlot(forest.hotel)


