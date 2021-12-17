library(dplyr)
#Imported the data frame
covid<- read.csv('time_series_covid_19_confirmed.csv')
#selected the columns im interested in


covid[is.na(covid)]<-0
cases<-covid%>%
  select(Country.Region,X3.23.20)%>%
  group_by(Country.Region)%>%
  arrange(desc(X3.23.20))
  

#combined all the cases for each counties region into one figure for each country
#This is preparing the data so it is more properly suited for the graphs i will make
cases[is.na(top15)]<-0
#some fields were empty and had N/A i replaced it with int 0

cases_per_country<-cases%>%group_by(Country.Region)%>%
  summarise(total_cases = sum(X3.23.20))%>%
  arrange(desc(total_cases))



cases_per_country

top15<-cases_per_country%>% slice(1:15)
bot15<-cases_per_country%>% arrange(-desc(total_cases))%>%slice(1:15)


library(ggplot2)
  
#Question 1 create graphs for top 15 and botom 15 countries

ggplot(data = top15,aes(x=reorder(Country.Region,-total_cases),y=total_cases))+
  geom_bar(stat = 'identity',fill='red')+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45,hjust = 0.9),plot.title=element_text(hjust=0.5),plot.subtitle = element_text(hjust = 0.5))+
  labs(title='Top 15 countries most recent Covid-19 cases',x='Countries',y='Case Numbers',
       subtitle = 'Cases as of March 23,2020')
# bottom 15
ggplot(data = bot15,aes(x=reorder(Country.Region,-total_cases),y=total_cases))+
  geom_bar(stat = 'identity',fill='red')+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45,hjust = 0.9),plot.title=element_text(hjust=0.5),plot.subtitle = element_text(hjust = 0.5))+
  labs(title='Bottom 15 countries most recent Covid-19 cases',x='Countries',y='Case Numbers',
       subtitle = 'Cases as of March 23,2020')

install.packages('rworldmap')
install.packages('tidyverse')
library(rworldmap)
library(tidyverse)

#data frame with all dates from Jan to March
all_countries_jan_mar<-covid%>%
  select(-Province.State,-Lat,-Long) %>% 
  group_by(Country.Region)%>%
  summarise_all(sum)

all_countries_jan_mar

mapdata <- map_data('world')
View(mapdata)


acjr<- rename(all_countries_jan_mar,region = Country.Region)

mapdata<-left_join(mapdata,acjr,by="region")
View(mapdata)

#remove N/A values and replace with 0
mapdata[is.na(mapdata)]<-0

map_x3_8_20<-mapdata%>%
  select(long,lat,group,order,region,subregion,X3.23.20)
#question 5.1
map1<-ggplot(map_x3_8_20,aes(x=long,y=lat,group=group,color=X3.23.20))+
  geom_polygon(aes(fill=X3.23.20))+
  labs(title='Global Representation of Covid-19 Cases',x='',y='',
       subtitle = 'Cases as of March 23, 2020')+
  theme(plot.title=element_text(hjust=0.5),plot.subtitle = 
  element_text(hjust = 0.5))
map1



#get the column sums for each day for all countries combined
daily_cases<- as.data.frame(colSums(acjr[,-1]))

#Format the table to remove char from date column, also gave
#columns names, so that they can be referenced in the future
daily_cases<-cbind(dates=rownames(daily_cases),daily_cases)
rownames(daily_cases)<- 1:nrow(daily_cases)
colnames(daily_cases) = c('dates','World')
daily_cases$dates <- gsub("^.{0,1}","0",daily_cases$dates)

#fix the date column
format(daily_cases$dates,format="%Y %m %d")

#Question 2
#build line plot to show time series of cases from January to march
daily_cases%>%
  ggplot(aes(x=reorder(dates,World),y=World,group=1))+
  geom_line(color='#D6121C')+
  scale_y_continuous(labels=function(x) format(x,scientific=FALSE))+
  theme_gray()+
  theme(axis.text.x = element_text(angle=90),
        plot.title=element_text(hjust=0.5),plot.subtitle = 
          element_text(hjust = 0.5))+
  annotate(geom = 'text',x='03.5.20',y=150000,label=
             'WHO declares Covid-19 a Pandemic')+
  annotate(geom = 'point',x='03.11.20',y=125865,size=6,shape=21,fill='blue')+
  labs(title='Confirmed Cases Worldwide ',x='Daily trend from January to March',y='Case Numbers ',
       subtitle = 'Data From Jan 22,2020 - Mar 23,2020')

#select China's daily covid cases
china<-acjr%>% filter(region=='China')

#converted the China to a vertical format so plotting could be easier
daily_china<- as.data.frame(colSums(china[,-1]))
daily_china<-cbind(dates=rownames(daily_china),daily_china)
rownames(daily_china)<- 1:nrow(daily_china)
colnames(daily_china) = c('dates','China')
daily_china$dates <- gsub("^.{0,1}","0",daily_china$dates)

#merge tables
library(reshape)
install.packages('reshape')
combined<-merge(daily_cases,daily_china,by='dates')
#long format
combined<- melt(combined,id='dates')
View(combined)



class(combined$dates)

#question 3
#combine both plots world and China

ggplot()+
  geom_line(data=combined,aes(x=reorder(dates,value),y=value,color=variable,group=variable))+
  scale_y_continuous(labels=function(x) format(x,scientific=FALSE))+
  theme_gray()+
  theme(axis.text.x = element_text(angle=90),
        plot.title=element_text(hjust=0.5),plot.subtitle = 
          element_text(hjust = 0.5))+
  annotate(geom = 'text',x='03.5.20',y=150000,label=
             'WHO declares Covid-19 a Pandemic')+
  annotate(geom = 'point',x='03.11.20',y=125865,size=6,shape=21,fill='blue')+
  labs(title='Cases in China vs World ',x='Daily trend from January to March',y='Case Numbers ',
       subtitle = 'Data From Jan 22,2020 - Mar 23,2020')

head(combined,10)
combined

#question4
combined <-combined%>%
  mutate(dates = as.Date(dates, format = "%m.%d.%y"))


combined%>%
  ggplot(aes(x=dates,y=value,color=variable,group=variable))+
  geom_line()+
  geom_smooth(data=subset(combined,variable=='China'),aes(dates,value),method='lm',se=FALSE,color='black')+
  scale_y_continuous(labels=function(x) format(x,scientific=FALSE))+
  theme_gray()+
  theme(axis.text.x = element_text(angle=90),
        plot.title=element_text(hjust=0.5),plot.subtitle = 
          element_text(hjust = 0.5))+
  labs(title='Cases in China vs World ',x='Daily trend from January to March',y='Case Numbers ',
       subtitle = 'Data From Jan 22,2020 - Mar 23,2020')
  

#part 5
#AN area graph showing the different regions in china and their case numbers


regions_in_China<-covid%>%
  select(-Long,-Lat)%>%
  filter(Country.Region=='China')
regions_in_China<- regions_in_China%>%
  select(-Country.Region)

#test
provinces<-melt(regions_in_China,id.vars = c('Province.State'))
#remove char and replace with 0
provinces$variable <- gsub("^.{0,1}","0",provinces$variable)
#convert column to ddate type
format(provinces$variable,format="%Y %m %d")


#plot stacked area chart
ggplot(provinces,
       aes(x=as.POSIXct(strptime(variable, format='%m.%d.%y')),
           y=value,
           fill=Province.State))+ 
  theme(plot.title=element_text(hjust=0.5))+
  labs(title = 'Area Chart of Different Regions in China',y='Covid Cases',x='')+geom_area()


