#linear regression aims to predict a continuous variable y from one or more dependent x variables.

if(! require (devtools))
  install.packages('MASS')

install.packages('MASS')
library(caret)  
library(tidyverse)

head(marketing,3)
data(swiss) 
head(swiss)
data(Boston)
head(Boston)


set.seed(123)

train.sample <- marketing$sales %>%
  createDataPartition(p=0.8,list=FALSE)

train.data <- marketing[train.sample,]
test.data <- marketing[-train.sample,]

#create model

model <- lm(sales ~.,data=train.data)

#summarize model
summary(model)

#make predictions

predictions <- model%>%
  predict(test.data)

#summary of predictions
summary(predictions)

#evaluate model
#RMSE root mean squared error

RMSE(predictions,test.data$sales)

#R2 r-squared
R2(predictions,test.data$sales)

#####
        #simple linear regression
######

model2 <-lm(sales~youtube,data=train.data)
summary(model2)

#predictions using budgets for youtube

newdata <- data.frame(youtube = c(0,1000))
model2%>%
  predict(newdata)

####
    # Multiple linear regression predict the outcome of y using multiple x variables
####

multimodel <- lm(sales~youtube+facebook+newspaper,data=train.data)
summary(multimodel)$coef

#to include all available variables in the model, use [ ~. ]
# larger the standard error, less conidence about the estimates

#new budget

newdata2 <- data.frame(youtube=2000,facebook=1000,newspaper=1000)
multimodel%>%
  predict(newdata2)

# After observing summary of model,
# it can be seen that newspaper will not affect sales significantly
#therefore, it can be removed from the model

#this is due to the low t value
# low t value indicates low predictive power of the coefficient

#model without newspaper variable

model4<- lm(sales~youtube+facebook,data=train.data)
summary(model4)
model4%>%predict(newdata2)

## Model accuracy

#after determining that at least one variable is highly associated with the outcome
# continue diagnosis by checking how well the model fit the data   GOODNESS-OF-FIT

#RSE residual standard error
#R-squared (R2) and adjusted R2
#F-statistic

#RSE represents the average difference b/n the observed outcome values and the predicted
#values by the model .THE LOWER THE RSE, THE BEST IT FITS OUR DATA

#DIVIDING RSE BY AVERAGE VALUE OF THE OUTCOME VARIABLE WILL GIVE PREDICTION ERROR RATE
# WHICH SHOULD BE LOW

#RSE/DATA$VARIABLE=KK%

pr_error<-2.038/mean(train.data$sales)*100
pr_error

###   R-Squared and adjusted R2 range 0 to 1
# Represents the variation in the outcome variable that can be explained
# by the model predictor variable
#THE HIGHER THE R2 THE BETTER THE MODEL
#MAINLY USE THE ADJUSTED R2
#AN ADJUSTED R2 CLOSER TO ONE INDICATES THAT A LARGE PROPORTION OF THE VIABILITY
  #IN THE OUTCOME HAS BEEN EXPLAINED BY THE REGRESSION MODEL
#A NUMBER NEAR 0 INDICATES THAT THE MODEL DID NOT EXPLAIN MUCH VIABILITY

###   F-STATISTIC
#THIS GIVES THE OVERALL SIGNIFICANCE OF THE MODEL
#THIS METHOD IS IMPORTANT IN MULTIPLE LINEAR REGRESSION
#LARGE F-STATISTIC CORRESPONDS TO STATISTICALLY SIGNIFICANT P-VALUE(P<0.05)


####        MAKING PREDICTIONS

#MAKE PREDICTIONS USING THE TEST DATA

#1 PREDICT THE VALUES BASED ON THE TEST DATA
#2 ASSES THE MODEL PERFORMANCE
  #RMSE
  #R2


#LINEAR REGRESSION ASSUMES A LINEAR RELATIONSHIP 
#TO CHECK IF RELATIONSHIP IS LINEAR, CREATE A SCATTER PLOT OF OUTCOME VARIABLE AND PREDICTOR VARIABLE

ggplot(data=marketing,aes(x=youtube,y=sales))+
  geom_point()+
  stat_smooth() 'TO ADD SMOOTHED LINE'

#INTERACTION EFFECTS IN MULTIPLE REGRESSION

#EQUATION Y=B0 + B1*X1 + B2*X2 +B3(X1+X2)
# interaction effects
model3 <- lm(sales~facebook*youtube,data=train.data)
summary(model3)

#make predictions
predictions2 <-model3%>%predict(test.data)
summary(predictions2)
RMSE(predictions2,test.data$sales)
R2(predictions,test.data$sales)

# if after evaluation of the model, if the interaction effect
#produces a better result, then choose the model with the interaction effects
