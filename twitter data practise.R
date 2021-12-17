install.packages('rtweet')
install.packages('tidytext')

library(rtweet)
library(ggplot2)
library(dplyr)
library(tidytext)
library(tidyr)
library(rjson)
library(stringr)

library(ggmap)

appname<-'covid_word'
key<-'CJgFmhQckyPI8HJlFk7nI6LGf'
secret<-''


twitter_token<- create_token(
  app= 'covid_word',
  consumer_key = 'CJgFmhQckyPI8HJlFk7nI6LGf',
  consumer_secret = 'Wd6OJSkcfKuY5VJWAFcgiLjrzxl9DKh3H2FRzwbENFEg4fM7Q0'
)

auth_setup_default()

df<-search_tweets('covid19',n=18000,lang='en',type = 'recent',include_rts = TRUE)

# sentiment analysis syuzhet package
library(syuzhet)
sa.value_date <- get_nrc_sentiment(df$text)

feel_sum<- colSums(sa.value_date[,])
feel_sum_df<-data.frame(feel_sum)
sa.feel_sum<-cbind(sentiment = row.names(feel_sum_df),feel_sum_df,row.names=NULL)


# pie chart showing sentiments
sa.feel_sum%>%ggplot(aes(x='',y=feel_sum,fill=sentiment))+
  geom_col(color='black')+ #control lines between pie portion
  geom_text(aes(label=feel_sum),position = position_stack(vjust = 0.5))+
  coord_polar(theta = 'y')+
  labs(title='Sentiment Chart for covid19',x='',y='',
       xticks=FALSE,yticks=FALSE,caption='Sentiments as of 11/27/2021',
       fill='Sentiments')+
  theme(plot.title = element_text(hjust = 0.5,colour='aquamarine3',size=15,face='bold'),
        legend.title = element_text(color='aquamarine3'),
        legend.box.background = element_rect(color='black'),
        axis.text.x = element_blank(),
        panel.background = ,
        )


post_tweet('testing')

sp<-search_tweets('#saopaulo',n=500)

tweets<- sp$text
as.data.frame(tweets)
library(stringr)
wlist<- c('marketing','bolso')

View(test<-sp%>%
       select(screen_name,text)%>%
       filter(str_detect(text,paste(wlist,collapse = '|'))))

tt<-get_timeline(test$screen_name,n=200)

vt<-tt%>%
  select(text,screen_name)%>%
  filter(screen_name=='em_trevo')
head(vt,10)

survivor<-search_tweets('survivor41 ',n=1000)

w1<-length(unique(survivor$location))
head(w1)

survivor%>%
  count(location,sort=TRUE)%>%
  mutate(location=reorder(location,n))%>%
  na.omit()%>%
  top_n(20)%>%
  ggplot(aes
         (x=location,y=n),na.omit(survivor))+
  geom_col()+
  coord_flip()
get_timeline(user='ailamar20')

rt<- search_tweets('#rstats',n=3000,include_rts = FALSE)
ts_plot(rt,by='1 hours')  

celebs<-get_timelines(c('KatyPerry','KimKardashian','arianagrande'),n=1000)

ts_plot(group_by(user_id),data=celebs,x=celebs$user_id)

celeb_graph<-celebs%>%
  group_by(screen_name)%>%
  ts_plot(by='week')

ggsave(filename='celebs_graph.png',dpi=300,height = 8,width = 16,units = 'in',device='png')+
  post_tweet('Testing post image and text',media='celebs_graph.png')

stream_tweets(q='brasil, brazil',timeout = 30,parse=FALSE,file_name = 'stream1.json')
str1<-parse_stream('stream1.json')
View(testdf)
#########################
#analyze unique words
bra<-search_tweets('weather ,new york',n=1000,include_rts = FALSE,type)
bra$text<-gsub('http.*','',bra$text)
bra$text<-gsub('https.*','',bra$text)
bra$text<-gsub('&amp;','&',bra$text)

bea_clean<-bra%>%
  select(text)%>%
  unnest_tokens(word,text)
data('stop_words')

cleaned_bra_tweets<-bea_clean%>%
  anti_join(stop_words)

cleaned_bra_tweets%>%
  count(word,sort = TRUE)%>%
  top_n(15)%>%
  mutate(word = reorder(word,n))%>%
  ggplot(aes(x=word,y=n))+
  geom_col()

########################
#analyze location
bra_loc<-separate(data=bra,col=geo_coords,
                  into = c('Latitude','Longitude'),
                  sep=',',remove=FALSE)

#remove parentheses
bra_loc$Latitude<-str_replace_all(bra_loc$Latitude,'[c(]','')
bra_loc$Longitude<-str_replace_all(bra_loc$Longitude,'[c)]','')

#store as numeric
bra_loc$Longitude <- as.numeric(bra_loc$Longitude)
bra_loc$Latitude <- as.numeric(bra_loc$Latitude)

#clean up NA
bra_loc <-subset(bra_loc,!is.na(Latitude)&!is.na(Longitude))

#set up empty map
bra_map<- get_map(location= c(lon=mean(c(-51.92528)),
                              lat=mean(c(-14.235004))),
                  zoom=1,maptype = 'terrain',source='osm')
