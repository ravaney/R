#residual errors and fittd values

library(tidyverse)
install.packages('broom')
library(broom)
theme_set(theme_classic())
library(ggplot2)

data('marketing',package = 'datarium')

head(marketing)
#sample the data
sample_n(marketing,3)

#build a regression model
model<- lm(sales~youtube,data=marketing)
model

summary(model)

model.diag.metrics <- augment(model)
head(model.diag.metrics)

ggplot(data=model.diag.metrics,aes(youtube,sales))+
  geom_point()+
  stat_smooth(method=lm,se=FALSE)+
  geom_segment(aes(xend=youtube,yend=.fitted),colour='red',size=0.3)

##Linear Regression Assumptions

#1 Linearity of the data - the relationship between the predictor
# (x) and the outcome (y) is assumed to be linear

#2 Normality of Residuals - The residual errors are assumed to be normally distributed

#3 Homogeneity of Residual variance - The residuals are assumed to have a constant variance (Homoscedasticity)

#4 Independence od Residuals error terms

#####
#     Potential Problems Include
#####

#1 Non-Linearity of the outcome - predictor relationship
#2 Heteroscedasticity - Non-constant variance of error terms
#3 Presence of influential values in the data can be:
  #i. Outliers: extreme values in the outcome (y) variable
  #ii. High-leverage points: extreme values in the predicttors (x) variable

#####
    # regression Diagnostics {reg-diag}
#####

#diagnostics plot using base r
par(mfrow=c(2,2))
plot(model)

#using ggfortify
install.packages('ggfortify')
library(ggfortify)
autoplot(model)

#the diagnostics plot show residuals in 4 ways

#1 Residuals vs Fitted - check linear relationship assumptions
  # a horizontal line without distinct patterns is a good indication for linear relationship

#2 Normal Q-Q - used to examine whether the residuals are normally distributed.
  # good if residuals follow a straight dashed line

#3 Scale-Location - or spread location is used to check homogeneity of variance of the residuals 
  # Horizontl line with equally spread points is a good indication of homoscedasticity

#4 Residuals vs leverage - used to identify influential cases, ie, extreme values that might influence the regression
  # results when included or excludedd from the analysis

#####
    # Linearity of the data
#####

# this can be checked by using the residuals vs fitted plot
plot(model,1)

# if the residual plot indicates non-linearity, use a non-linear method such as log(x),
# sqrt(x), and x^2

## Homogeneity of Variance 
    # This can be checcked by examining the scale-location plot aka spread-location plot
    # if a horizontal line is shown, then the model is good, if otherwise
    # a possible solution is to use log or square root transformation of the outcome variable (y)
plot(model,3)

#solution use log on the outcome variable
model2 <- lm(log(sales)~youtube, data=marketing)
plot(model2,3)

#####
    # Normality of Residuals
#####

# The QQ plot of residuals can be used to visually check the normality of assumption
# It should follow a straight line
plot(model,2)

#####
    # Outliers and high leverag points
#####

# an outlier is a point that has an extreme outcome variable value. WHich may affect the interpretation of the model, because it
# increases RSE. Outliers can be identifued using the standardised residual

# High leverage points - a data point has high leverage if it has extreme predictor x variables
# this can be detected by examining the leverage statistic ot hat-value. a value of this statistic above 
# 2(p +1)/n indicates an observation with high leverage, where p is the number of predictors and n is the number of 
# observations

#observations whose standardized residuals are greater than 3 in absolute value are possible outliers

plot(model,5)

#####
    # Influential Values
#####

# this is a value whise inclusion or exclusion can alter resluts or the reg analysis. They are asociated with
# a large residual.   Not all outliers are influential in linear regression analysis

#cook's distance determine the influence of a value
# the residuals vs leverage plot can help find any influential observations.
# outliers usually stay in the ypper right or lower right

plot(model,4)#cook's distance
plot(model,5)
