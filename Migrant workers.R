library(rtweet)
library(RColorBrewer)
library(NLP)
library(SnowballC)
library(ggplot2)
library(tm)
library(wordcloud)
library(stringr)

# whatever name you assigned to your created app

appname="MigrantLabour"

#Your api key
api_key="P***********************3"
api_secret="h****************************2"

##create twiter_token variable with help of app,consumer_key and consumer_secret

twitter_token = create_token(
  app = "MigrantLabour",
  consumer_key = api_key,
  consumer_secret = api_secret)

#search tweet
mw=search_tweets("MigrantWorkers,",
                 n=3000, since='2020-03-23',lang="en",
                 token = twitter_token)
View(mw)

install.packages("ggplot2", dependencies = TRUE, INSTALL_opts = '--no-lock')

head(mw$text)

#converted to dataframe
mw1=as.data.frame(mw)

#remove emoticons
mw1$text <- sapply(mw1$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

#create corpus
mw1_corpus<- Corpus(VectorSource(as.vector(mw1$text)))


#lowercase
mw1_corpus1<-tm_map(mw1_corpus,content_transformer(tolower))

#remove puntuation
mw1_corpus1<-tm_map(mw1_corpus1,content_transformer(removePunctuation))

#remove numbers
mw1_corpus1<-tm_map(mw1_corpus1,content_transformer(removeNumbers))


#remove bad text

text_processing<- function(x)
{gsub("http[[:alnum:]]*",'', x)
  gsub('http\\S+\\s*', '', x) ## Remove URLs
  gsub('\\b+RT', '', x) ## Remove RT
  gsub('#\\S+', '', x) ## Remove Hashtags
  gsub('@\\S+', '', x) ## Remove Mentions
  gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
  gsub("\\d", '', x) ## Remove Controls and special characters
  gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
  gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
  gsub(' +',' ',x) ## Remove extra whitespaces
}

mw1_corpus1<-tm_map(mw1_corpus1,text_processing)

inspect(mw1_corpus1)


#add stopwords
mywords<-c(stopwords("english"),"migrantworkers","help","httpstcogteveemmnl","india","think","rt","íí","get","like","just","yes","know","will",
           "httpstcogtlnlivn","good","day","people","httpstcorctfgwgqhq","httpstcowkhacuzfw","amp")

#remove stopwords
mw1_corpus1<-tm_map(mw1_corpus1,removeWords,mywords)

mw2<-data.frame(text=sapply(mw1_corpus1, identity), 
                stringsAsFactors=F)

inspect(mw1_corpus1)


#term document

my_dtm <-TermDocumentMatrix(mw1_corpus1)
inspect(my_dtm[1:5,3:8])

m<-as.matrix(my_dtm)
v<-sort(rowSums(m),decreasing = TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,10)
tail(d,10)             
#generate wordcloud
set.seed(1234)

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=300, random.order=FALSE, rot.per=.25, 
          colors=brewer.pal(15, "Dark2"))

head(d)

# order bars
top= top_n(d,50)

p1<- ggplot(top, aes(x = reorder(word,freq), y = freq)) +
  geom_col() +
  xlab(NULL)+
  coord_flip() +
  theme_classic()+
  labs(x="Count",
       y="Unique words",
       title= "Unique words counts found in #Migrantlabour tweet")
 
plot(p1)


##nrc sentiment analysis

nrc_word= d%>%inner_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment,sort = TRUE)

ggplot(nrc_word, aes(x = reorder(sentiment,n), y = n)) +
  geom_col(fill="steelblue") +
  labs(x="Sentiment",y=NULL)+
  ggtitle("Setiment distribution of '#Migrantworker' tweet")+
  coord_flip()

nrc_word1= d%>%inner_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment)) %>%
  count(word,sentiment,sort = TRUE)

d2 <- d %>%
    inner_join(nrc_word1)
d2_pos<-filter(d2,sentiment=="positive")
d2_neg<-filter(d2,sentiment=="negative")

#Word cloud for positive sentiment
wordcloud(words = d2_pos$word, freq = d2_pos$freq, min.freq = 1,
          max.words=300, random.order=FALSE, rot.per=.35, 
          colors=brewer.pal(20, "Set1"))


#Word cloud for negative sentiment
wordcloud(words = d2_neg$word, freq = d2_neg$freq, min.freq = 1,
          max.words=300, random.order=FALSE, rot.per=.25, 
          colors=brewer.pal(15, "Dark2"))


