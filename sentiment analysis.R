#Omicron Sentiment analsis
library(dplyr)

omicron<- search_tweets('uk',n=18000,type='recent',lang='en', include_rts = FALSE)

tweets.omicron <- omicron%>%select(screen_name,text)

tweets.omicron$text <-gsub('http.*','',tweets.omicron$text)
tweets.omicron$text <-gsub('https.*','',tweets.omicron$text)
tweets.omicron$text <-gsub('&amp*','&',tweets.omicron$text)

tweets.omicron <-tweets.omicron%>%select(text)%>%
  unnest_tokens(word,text)

data('stop_words')

# find top 10 most used words
cleaned_tweets.omicron <- tweets.omicron %>%
  anti_join(stop_words)

cleaned_tweets.omicron%>%
  count(word,sort=TRUE)%>%
  top_n(10)%>%
  mutate(word=reorder(word,n))%>%
  ggplot(aes(x=word,y=n))+
  geom_col()+
  geom_text(aes(label=n),color='black')


