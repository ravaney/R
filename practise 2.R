library(dplyr)
library(nycflights13)
library(ggplot2)

View(flights)

test <- filter(flights,month==07,day==3)
View(test)

View(mutated_column<-mutate(flights,overall_delay=arr_delay-dep_delay))

('mtcars')
View(mtcars)

ggplot(mtcars,aes(x=mpg,y=drat))+
  geom_point(color='black',fill='darkblue')+
  geom_line(color='darkblue')+
  geom_line(size=0.7,color='green',aes(mpg,wt))+
  geom_point(aes(x=mpg,y=wt),color='orange')+
  geom_point(color='orange',aes(mpg,wt,lwd=0.01))+
  geom_smooth(color='red',method='loess',aes(x=mpg,y=drat))+
  theme_minimal()
install.packages('quantmod')

library('tidyquant')
(tidyquant)
install.packages('tidyquant')
library(tidyquant)
getSymbols('AAPL',from = '2021-01-01',to = '2021-09-23',warning=FALSE,auto.sign=TRUE)

head(AAPL)
View(AAPL)
AAPL<-cbind(rownames(AAPL),AAPL)
rownames(AAPL)<-NULL
colnames(AAPL)[0]<-'Dates'
View(AAPL)

AAPL<-fortify.zoo(AAPL)
names(AAPL)[1]<-'Dates'

AAPL%>%
  mutate(Dates=format(as.Date,'%y/%m/%d'))

ggplot(AAPL,aes(x=Dates,y=AAPL.Open))+
  geom_line(aes(x=Dates,y=AAPL.Open),color='blue')+
  geom_line(aes(x=Dates,y=AAPL.Close),color='red')+
  geom_smooth(method='lm')
