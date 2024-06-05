
##### LOAD PACKAGES
{
  library("academictwitteR")
  library("writexl")
  library("readxl")
  library("graphics")
  library("purrr")
  library("stringr") 
  library("tm")
  library("syuzhet")
  library("ggplot2")
  library("CGPfunctions")
  library("lsr")
  library("dplyr")
  library("summarytools")
  library("tidyr")
  library("scales")
  library("wordcloud")
  library("reshape2")
  library("wordcloud")
  library("stringr")
  library("widyr")
  library("topicmodels")
  library("arules")
}

tweets <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/raw/NewsArticles_ALL.xlsx")


##### STEP 5: Moving data to 'twitterCorpus' file to clean dataframe $text
#```{r}
twitterCorpus <-Corpus(VectorSource(tweets$article_text))
#inspect(twitterCorpus[1:10])
twitterCorpus<- tm_map(twitterCorpus, content_transformer(tolower))
twitterCorpus<- tm_map(twitterCorpus,removeWords,stopwords("en"))
twitterCorpus<- tm_map( twitterCorpus,removeNumbers)
twitterCorpus<- tm_map( twitterCorpus,removePunctuation)

removeURL<- function(x) gsub("http[[:alnum:]]*", "", x)   
twitterCorpus<- tm_map(twitterCorpus,content_transformer(removeURL))

#removeNonAscii<-function(x) textclean::replace_non_ascii(x) 
#twitterCorpus<-tm_map(twitterCorpus,content_transformer(removeNonAscii))

twitterCorpus<- tm_map(twitterCorpus,removeWords,c("amp","ufef",
                                                   "ufeft","uufefuufefuufef","uufef","s"))

twitterCorpus<- tm_map(twitterCorpus,stripWhitespace)

#inspect(twitterCorpus[1:10])


##### STEP 6: Moving Cleaned 'TwitterCorpus' Data to a Seperate Dataframe
dataframe <- data.frame(text=sapply(twitterCorpus, identity), 
                        stringsAsFactors=F)

##### STEP 7: Export cleaned dataframe to excel
#Replace clean text with old text
tweets <- bind_cols(tweets$publish_date, tweets$primary_author, tweets$article_title, tweets$article_text, dataframe$text, tweets$article_keywords, tweets$source_name, tweets$source_link, tweets$query, tweets$year, tweets$source)
colnames(tweets)[1] <- "created_at"
colnames(tweets)[2] <- "author_id"
colnames(tweets)[3] <- "article_title"
colnames(tweets)[4] <- "org_text"
colnames(tweets)[5] <- "new_text"
colnames(tweets)[6] <- "keywords"
colnames(tweets)[7] <- "source_name"
colnames(tweets)[8] <- "source_link"
colnames(tweets)[9] <- "query"
colnames(tweets)[10] <- "year"
colnames(tweets)[11] <- "source"


##### STEP 9: Generate Sentiment Scores as '$sentiment' using syuzhet method
get_sentiment(tweets$new_text[1:10])
sentiment <- get_sentiment(tweets$new_text)
sentimentTweets <- dplyr::bind_cols(tweets,data.frame(sentiment))

##### STEP 10: Determining the Mean of Sentiment Positivity
meanSent<-function(i,n){
  mean(sentimentTweets2$sent[i:n])
}

##### STEP 11: Determining the Sentiment Scores for all of the Tweets
#scores<-c(ALL=meansentiment(1,1817))

##### STEP 12: Create a rough geom_point Graph depicting the Sentiment Scores over time
ggplot(tweets, aes(x=created_at, y=sentiment)) +
  geom_point( aes(alpha = 1), stat = "identity", show.legend = TRUE) +
  labs(x='Date (Years)', y='Tweet Sentiment Score',
       title = 'Sentiment in Tweets Since July 2018',
       subtitle = 'search query = city park')

##### STEP 13: Creating a Regular Sentiment Score using get_sentiment() Function
# ***please note: different methods may have different scales.***
# see the first row of the vector
# see summary statistics of the vector
syuzhet_vector <- get_sentiment(sentimentTweets$new_text, method="syuzhet")
head(syuzhet_vector)
summary(syuzhet_vector)

# bing vector
bing_vector <- get_sentiment(sentimentTweets$new_text, method="bing")
head(bing_vector)
summary(bing_vector)
sentimentTweets <- dplyr::bind_cols(sentimentTweets,data.frame(bing_vector))

#affin vector
afinn_vector <- get_sentiment(sentimentTweets$new_text, method="afinn")
head(afinn_vector)
summary(afinn_vector)
sentimentTweets <- dplyr::bind_cols(sentimentTweets,data.frame(afinn_vector))

#nrc vector
nrc_vector <- get_sentiment(sentimentTweets$new_text, method="nrc")
head(nrc_vector)
summary(nrc_vector)
sentimentTweets <- dplyr::bind_cols(sentimentTweets,data.frame(nrc_vector))

#compare the first row of each vector using sign function
rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector)))

##### STEP 14: Run NRC Sentiment Analysis to Return Dataframe with Each Row Classified as emotions rather than score.
##### **anger, anticipation, disgust, fear, joy, sadness, surprise, trust**
##### **Also counts the number of positive and negative emotions found / row.
d <- get_nrc_sentiment(sentimentTweets$new_text)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d,10)

##### STEP 15: Bind NRC Sentiment Analysis to Dataframe
sentimentTweets <- dplyr::bind_cols(sentimentTweets,data.frame(d))


##### STEP 16: Identify Mean and StDev within '$sent' to Determine a 95% Confidence Interval
summary(sentimentTweets$sentiment)
pos.neu.neg <- factor(sign(sentimentTweets$sentiment), (-1):1, c('negative', 'neutral', 'positive'))
final_tweets <- dplyr::bind_cols(sentimentTweets,data.frame(pos.neu.neg))




#Save the analysis
write_xlsx(final_tweets,
           "/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/NewsArticles_ALL.xlsx")



