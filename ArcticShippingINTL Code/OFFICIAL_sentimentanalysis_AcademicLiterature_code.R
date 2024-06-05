

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

tweets <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/raw/AcademicLiterature_ALL.xlsx")


##### STEP 5: Moving data to 'twitterCorpus' file to clean dataframe $text
#```{r}
twitterCorpus <-Corpus(VectorSource(tweets$article_abstract))
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

dataframe <- data.frame(text=sapply(twitterCorpus, identity), 
                          stringsAsFactors=F)

#inspect(twitterCorpus[1:10])

twitterCorpus2 <-Corpus(VectorSource(tweets$article_text))
#inspect(twitterCorpus[1:10])
twitterCorpus2<- tm_map(twitterCorpus2, content_transformer(tolower))
twitterCorpus2<- tm_map(twitterCorpus2,removeWords,stopwords("en"))
twitterCorpus2<- tm_map( twitterCorpus2,removeNumbers)
twitterCorpus2<- tm_map( twitterCorpus2,removePunctuation)

removeURL<- function(x) gsub("http[[:alnum:]]*", "", x)   
twitterCorpus2<- tm_map(twitterCorpus2,content_transformer(removeURL))

#removeNonAscii<-function(x) textclean::replace_non_ascii(x) 
#twitterCorpus<-tm_map(twitterCorpus,content_transformer(removeNonAscii))

twitterCorpus2<- tm_map(twitterCorpus2,removeWords,c("amp","ufef",
                                                   "ufeft","uufefuufefuufef","uufef","s"))

twitterCorpus2<- tm_map(twitterCorpus2,stripWhitespace)


##### STEP 6: Moving Cleaned 'TwitterCorpus' Data to a Seperate Dataframe
dataframe_2 <- data.frame(text=sapply(twitterCorpus2, identity), 
                        stringsAsFactors=F)

##### STEP 7: Export cleaned dataframe to excel
#Replace clean text with old text
tweets <- bind_cols(tweets$publish_date, tweets$primary_author, tweets$article_title, tweets$article_abstract, dataframe$text, tweets$article_text, dataframe_2$text, tweets$article_keywords, tweets$source_name, tweets$source_link, tweets$query, tweets$year, tweets$source)
colnames(tweets)[1] <- "created_at"
colnames(tweets)[2] <- "author_id"
colnames(tweets)[3] <- "article_title"
colnames(tweets)[4] <- "org_abstract"
colnames(tweets)[5] <- "new_abstract"
colnames(tweets)[6] <- "org_text"
colnames(tweets)[7] <- "new_text"
colnames(tweets)[8] <- "keywords"
colnames(tweets)[9] <- "source_name"
colnames(tweets)[10] <- "source_link"
colnames(tweets)[11] <- "query"
colnames(tweets)[12] <- "year"
colnames(tweets)[13] <- "source"


##### STEP 9: Generate Sentiment Scores as '$sentiment' using syuzhet method
get_sentiment(tweets$new_abstract[1:10])
sentiment1 <- get_sentiment(tweets$new_abstract)
sentimentTweets2 <- dplyr::bind_cols(tweets,data.frame(sentiment1))

##### STEP 10: Determining the Mean of Sentiment Positivity
meanSent<-function(i,n){
  mean(sentimentTweets2$sent[i:n])
}

##### STEP 11: Determining the Sentiment Scores for all of the Tweets
#scores<-c(ALL=meansentiment(1,1817))

##### STEP 12: Create a rough geom_point Graph depicting the Sentiment Scores over time
ggplot(tweets, aes(x=created_at, y=sentiment1)) +
  geom_point( aes(alpha = 1), stat = "identity", show.legend = TRUE) +
  labs(x='Date (Years)', y='Tweet Sentiment Score',
       title = 'Sentiment in Tweets Since July 2018',
       subtitle = 'search query = city park')

##### STEP 13: Creating a Regular Sentiment Score using get_sentiment() Function
# ***please note: different methods may have different scales.***
# see the first row of the vector
# see summary statistics of the vector
syuzhet_vector1 <- get_sentiment(sentimentTweets2$new_abstract, method="syuzhet")
head(syuzhet_vector1)
summary(syuzhet_vector1)

# bing vector
bing_vector1 <- get_sentiment(sentimentTweets2$new_abstract, method="bing")
head(bing_vector1)
summary(bing_vector1)
sentimentTweets <- dplyr::bind_cols(sentimentTweets2$new_abstract,data.frame(bing_vector1))

#affin vector
afinn_vector1 <- get_sentiment(sentimentTweets2$new_abstract, method="afinn")
head(afinn_vector1)
summary(afinn_vector1)
sentimentTweets2 <- dplyr::bind_cols(sentimentTweets2,data.frame(afinn_vector1))

#nrc vector
nrc_vector1 <- get_sentiment(sentimentTweets2$new_abstract, method="nrc")
head(nrc_vector1)
summary(nrc_vector1)
sentimentTweets2 <- dplyr::bind_cols(sentimentTweets2,data.frame(nrc_vector1))

#compare the first row of each vector using sign function
rbind(
  sign(head(syuzhet_vector1)),
  sign(head(bing_vector1)),
  sign(head(afinn_vector1)))

##### STEP 14: Run NRC Sentiment Analysis to Return Dataframe with Each Row Classified as emotions rather than score.
##### **anger, anticipation, disgust, fear, joy, sadness, surprise, trust**
##### **Also counts the number of positive and negative emotions found / row.
d1 <- get_nrc_sentiment(sentimentTweets2$new_abstract)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d1,10)

##### STEP 15: Bind NRC Sentiment Analysis to Dataframe
sentimentTweets2 <- dplyr::bind_cols(sentimentTweets2,data.frame(d1))


##### STEP 16: Identify Mean and StDev within '$sent' to Determine a 95% Confidence Interval
summary(sentimentTweets2$sentiment1)
pos.neu.neg <- factor(sign(sentimentTweets2$sentiment1), (-1):1, c('negative', 'neutral', 'positive'))
final_tweets1 <- dplyr::bind_cols(sentimentTweets2,data.frame(pos.neu.neg))




###
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
           "/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/JournalArticles_DISCUSSION_ALL.xlsx")

write.csv(final_tweets1,
           "/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/JournalArticles_Abstract_ALL.csv")

