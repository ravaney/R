library(scales)
library(ggplot2)
library(dplyr)
library(reshape)
library(ggpubr)

install.packages("ggpubr")

trade<-read.table('c:/users/lewis/desktop/trade.txt',header=TRUE,sep = '\t')



trade<- as.data.frame(trade)
trade<-melt(trade)

colnames(trade)<-c('countries','year','value')

trade <-subset(trade,select = -x)
trade_copy<-trade
trade<-trade_copy

trade$year<-gsub("[^0-9-]", "", trade$year)#remove non numeric


trade$year<-as.integer(trade$year)
trade$value<-as.integer(trade$value)

trade<-trade%>%select(countries,year,value)%>%
  mutate(value=value*1000)

ggplot(trade,aes(x=year,y=value,color=countries))+
  geom_line(show.legend = FALSE)+
  geom_bar(stat='identity',aes(fill=countries),color='white',width=0.8)+
  facet_wrap(~countries,ncol=3,scales='free')+
  scale_y_continuous(labels=dollar)+
  labs(title='Dollar value of Dairy products Imported by Jamaica'
       ,y='Amount in $USD',x='Years',xticks=TRUE,fill='Countries')+
  theme(plot.title = element_text(hjust=0.5))

  
#pie chart showing composition of market share of dairy market for jamaica
trade%>%filter(countries !='World',value !=0)%>%
  ggplot(aes(x='',y=value,fill=factor(countries)),color='black')+
  geom_bar(width=1,stat='identity',color='black')+
  scale_y_continuous(labels=dollar)+
  geom_text(aes(label=dollar(value)),position = position_stack(vjust = 0.5))+
  my_cp+
  facet_wrap(~year,ncol=5,scales='free')+
  labs(fill='Countries',title = 'Countries that export Dairy Products to Jamaica' ,x='')+
  theme(aspect.ratio = 1, 
        axis.ticks=element_blank(),
        legend.direction = 'horizontal',
        legend.position = 'bottom',
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=20),
        legend.title = element_text(size=30),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5,size=30),
        axis.text.y = element_text(size=30))+
  guides(fill=guide_legend(title.position = 'top',nrow=1))

a<-trade%>%filter(countries != 'World')%>%
  ggplot(aes(x=year,y=value,fill=countries,group=countries))+
  geom_line(size=1,aes(color=countries))+
  scale_y_continuous(labels=dollar)+
  labs(color='Countries',x='Year',y='Dollar Value', title =
         'Jamaica\'s Annual Dairy Imports')+
  theme(plot.title = element_text(hjust=0.5,size = 20))


b<-trade%>%filter(countries != 'World')%>%
  ggplot(aes(x=as.numeric(year),y=value))+
  geom_point(aes(shape=countries,color=countries,size=countries),show.legend = FALSE)+
  geom_smooth(se=F,aes(group=countries,color=countries))+
  scale_y_continuous(labels=dollar)+
  labs(title='Linear Representation of Jamaica\'s Dairy Imports',
       x='Year',y='Dollar Value',color='Countries')+
  theme(plot.title = element_text(hjust = 0.5,
                                  size=20))

c<-trade%>%filter(countries !='World',value>0)%>%
  ggplot(aes(x=year,y=value))+
  geom_bar(stat='identity',aes(fill=countries),position=position_dodge())+
  scale_y_continuous(labels = dollar)+
  stat_summary(fun = sum,aes(label=sprintf('$%1.2f ',..y..),group=year,vjust=5),geom='text')+
  labs(title='Composition of Market Share for Dairy in the Jamaican Market',
       x='Year',y='Dollar Value',fill='Countries')+
  theme(plot.title = element_text(hjust=0.5,
                                  size=20))

#instead of stacked bars use position=position_dodge to put them side by side

ggarrange(a,b,c,labels=c(),ncol=2,nrow=2)

#goem_bar or geom_col may be used


trade%>%filter(countries != 'World')%>%
  group_by(year)%>%
  summarise(sum(value))

my_cp<-coord_polar(theta = 'y')
my_cp$is_free <- function()TRUE

str(trade)  g

summary(trade)

head(trade,10)

