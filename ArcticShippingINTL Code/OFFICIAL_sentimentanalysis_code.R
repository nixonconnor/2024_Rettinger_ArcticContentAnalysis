
ug.twitter <-
  get_all_tweets(
    query = "arctic icebreaker",
    start_tweets = "2007-01-01T00:00:00Z",
    end_tweets = "2022-12-31T00:00:00Z",
    data_path = "/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/raw/CH/arcticicebreaker.tweet",
    bind_tweets = FALSE,
    bearer_token = "AAAAAAAAAAAAAAAAAAAAAG6ijQEAAAAA5K8KAJqnBUFykDqLXZc%2BYPMfZ3Q%3Dv8t6lSN0hx3IxfWlU2Bxs7BhdzL5LyleplHtzTh7zn52KkMbgk",
    country = "CH",
    lang = "en",
    n=Inf )

tweets <- bind_tweets(
  data_path = "/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/raw/CH/arcticicebreaker.tweet")


#tweets <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CA_arcticshipping.xlsx")
#tweets <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CA_arctictrade.xlsx")

tweets <- bind_cols(tweets$created_at, tweets$author_id, tweets$text, tweets$source, tweets$lang, tweets$possibly_sensitive)
colnames(tweets)[1] <- "created_at"
colnames(tweets)[2] <- "author_id"
colnames(tweets)[3] <- "text"
colnames(tweets)[4] <- "source"
colnames(tweets)[5] <- "lang"
colnames(tweets)[6] <- "possibly_sensitive"

##### STEP 5: Moving data to 'twitterCorpus' file to clean dataframe $text
#```{r}
twitterCorpus <-Corpus(VectorSource(tweets$text))
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
#write_xlsx(dataframe,"/Users/connornixon/Desktop/academictwitteR/data/clean.data/data2/US/publicfield.tweet.cleanedtweets.xlsx")

#Replace clean text with old text
tweets <- bind_cols(tweets$created_at, tweets$author_id, tweets$text, dataframe$text, tweets$source, tweets$lang, tweets$possibly_sensitive)
colnames(tweets)[1] <- "created_at"
colnames(tweets)[2] <- "author_id"
colnames(tweets)[3] <- "org_text"
colnames(tweets)[4] <- "new_text"
colnames(tweets)[5] <- "source"
colnames(tweets)[6] <- "lang"
colnames(tweets)[7] <- "possibly_sensitive"

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

##### Choose specific country
#final_tweets$Country <- "Canada"
#final_tweets$Country <- "Denmark"
#final_tweets$Country <- "Finland"
#final_tweets$Country <- "Iceland"
#final_tweets$Country <- "Norway"
#final_tweets$Country <- "Russia"
#final_tweets$Country <- "Sweden"
#final_tweets$Country <- "United States"

#final_tweets$Country <- "Germany"
#final_tweets$Country <- "Netherlands"
#final_tweets$Country <- "Poland"
#final_tweets$Country <- "Great Britain"
#final_tweets$Country <- "France"
#final_tweets$Country <- "Spain"
#final_tweets$Country <- "Italy"
#final_tweets$Country <- "Japan"
#final_tweets$Country <- "China"
#final_tweets$Country <- "India"
#final_tweets$Country <- "South Korea"
#final_tweets$Country <- "Singapore"
final_tweets$Country <- "Switzerland"


class(final_tweets$created_at)
final_tweets$created_at <- as.Date(final_tweets$created_at)
class(final_tweets$created_at)
k07 <- final_tweets[final_tweets$created_at >= "2007-01-01" & final_tweets$created_at <= "2007-12-31", ]
k07$year <- "2007"
k08 <- final_tweets[final_tweets$created_at >= "2008-01-01" & final_tweets$created_at <= "2008-12-31", ]
k08$year <- "2008"
k09 <- final_tweets[final_tweets$created_at >= "2009-01-01" & final_tweets$created_at <= "2009-12-31", ]
k09$year <- "2009"
k10 <- final_tweets[final_tweets$created_at >= "2010-01-01" & final_tweets$created_at <= "2010-12-31", ]
k10$year <- "2010"
k11 <- final_tweets[final_tweets$created_at >= "2011-01-01" & final_tweets$created_at <= "2011-12-31", ]
k11$year <- "2011"
k12 <- final_tweets[final_tweets$created_at >= "2012-01-01" & final_tweets$created_at <= "2012-12-31", ]
k12$year <- "2012"
k13 <- final_tweets[final_tweets$created_at >= "2013-01-01" & final_tweets$created_at <= "2013-12-31", ]
k13$year <- "2013"
k14 <- final_tweets[final_tweets$created_at >= "2014-01-01" & final_tweets$created_at <= "2014-12-31", ]
k14$year <- "2014"
k15 <- final_tweets[final_tweets$created_at >= "2015-01-01" & final_tweets$created_at <= "2015-12-31", ]
k15$year <- "2015"
k16 <- final_tweets[final_tweets$created_at >= "2016-01-01" & final_tweets$created_at <= "2016-12-31", ]
k16$year <- "2016"
k17 <- final_tweets[final_tweets$created_at >= "2017-01-01" & final_tweets$created_at <= "2017-12-31", ]
k17$year <- "2017"
k18 <- final_tweets[final_tweets$created_at >= "2018-01-01" & final_tweets$created_at <= "2018-12-31", ]
k18$year <- "2018"
k19 <- final_tweets[final_tweets$created_at >= "2019-01-01" & final_tweets$created_at <= "2019-12-31", ]
k19$year <- "2019"
k20 <- final_tweets[final_tweets$created_at >= "2020-01-01" & final_tweets$created_at <= "2020-12-31", ]
k20$year <- "2020"
k21 <- final_tweets[final_tweets$created_at >= "2021-01-01" & final_tweets$created_at <= "2021-12-31", ]
k21$year <- "2021"
k22 <- final_tweets[final_tweets$created_at >= "2022-01-01" & final_tweets$created_at <= "2022-12-31", ]
k22$year <- "2022"

final_tweets <- rbind(k07, k08, k09, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21, k22)

#add query to the dataset
#final_tweets$query <- "arctic shipping"
#final_tweets$query <- "arctic council"
#final_tweets$query <- "northwest passage"
#final_tweets$query <- "arctic passage"
final_tweets$query <- "arctic icebreaker"

#final_tweets$query <- "northeast passage"
#final_tweets$query <- "NW passage"
#final_tweets$query <- "arctic trade"

#Add membership status as column
#final_tweets$status <- "official member"
final_tweets$status <- "observer state"

#Save the analysis
write_xlsx(final_tweets,
           "/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CH_arcticicebreaker.xlsx")







### Binding Queries together
#####
#ArcticShipping
CA <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CA_arcticshipping.xlsx")
DK <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/DK_arcticshipping.xlsx")
FI <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/FI_arcticshipping.xlsx")
IS <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/IS_arcticshipping.xlsx")
NO <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/NO_arcticshipping.xlsx")
RU <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/RU_arcticshipping.xlsx")
SE <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/SE_arcticshipping.xlsx")
US <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/US_arcticshipping.xlsx")

DE <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/DE_arcticshipping.xlsx")
NL <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/NL_arcticshipping.xlsx")
PL <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/PL_arcticshipping.xlsx")
GB <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/GB_arcticshipping.xlsx")
FR <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/FR_arcticshipping.xlsx")
ES <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/ES_arcticshipping.xlsx")
IT <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/IT_arcticshipping.xlsx")
JP <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/JP_arcticshipping.xlsx")
CN <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CN_arcticshipping.xlsx")
IN <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/IN_arcticshipping.xlsx")
#KR <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/KR_arcticshipping.xlsx")
SG <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/SG_arcticshipping.xlsx")
CH <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CH_arcticshipping.xlsx")

ArcticShipping <- rbind(CA,DK,FI,IS,NO,RU,SE,US,DE,NL,PL,GB,FR,ES,IT,JP,CN,IN,SG,CH)
write_xlsx(ArcticShipping, "/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/aa_groupings/arcticshipping.xlsx")

#ArcticTrade
CA <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CA_arctictrade.xlsx")
DK <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/DK_arctictrade.xlsx")
FI <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/FI_arctictrade.xlsx")
IS <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/IS_arctictrade.xlsx")
NO <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/NO_arctictrade.xlsx")
RU <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/RU_arctictrade.xlsx")
SE <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/SE_arctictrade.xlsx")
US <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/US_arctictrade.xlsx")

DE <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/DE_arctictrade.xlsx")
NL <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/NL_arctictrade.xlsx")
#PL <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/PL_arctictrade.xlsx")
GB <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/GB_arctictrade.xlsx")
FR <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/FR_arctictrade.xlsx")
ES <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/ES_arctictrade.xlsx")
IT <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/IT_arctictrade.xlsx")
#JP <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/JP_arctictrade.xlsx")
#CN <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CN_arctictrade.xlsx")
IN <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/IN_arctictrade.xlsx")
#KR <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/KR_arctictrade.xlsx")
#SG <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/SG_arctictrade.xlsx")
CH <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CH_arctictrade.xlsx")

ArcticTrade <- rbind(CA, DK, FI, IS, NO, RU, SE, US, DE, NL, GB, FR,ES, IT, IN, CH)
write_xlsx(ArcticTrade, "/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/aa_groupings/arctictrade.xlsx")

#ArcticCouncil
CA <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CA_arcticcouncil.xlsx")
DK <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/DK_arcticcouncil.xlsx")
FI <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/FI_arcticcouncil.xlsx")
IS <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/IS_arcticcouncil.xlsx")
NO <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/NO_arcticcouncil.xlsx")
RU <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/RU_arcticcouncil.xlsx")
SE <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/SE_arcticcouncil.xlsx")
US <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/US_arcticcouncil.xlsx")

DE <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/DE_arcticcouncil.xlsx")
NL <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/NL_arcticcouncil.xlsx")
PL <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/PL_arcticcouncil.xlsx")
GB <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/GB_arcticcouncil.xlsx")
FR <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/FR_arcticcouncil.xlsx")
ES <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/ES_arcticcouncil.xlsx")
IT <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/IT_arcticcouncil.xlsx")
JP <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/JP_arcticcouncil.xlsx")
CN <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CN_arcticcouncil.xlsx")
IN <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/IN_arcticcouncil.xlsx")
#KR <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/KR_arcticcouncil.xlsx")
SG <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/SG_arcticcouncil.xlsx")
CH <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CH_arcticcouncil.xlsx")



ArcticCouncil <- rbind(CA,DK,FI,IS,NO,RU,SE,US,DE,NL,PL,GB,FR,ES,IT,JP,CN,IN,SG,CH)
write_xlsx(ArcticCouncil, "/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/aa_groupings/arcticcouncil.xlsx")

#NorthwestPassage
CA <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CA_northwestpassage.xlsx")
DK <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/DK_northwestpassage.xlsx")
FI <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/FI_northwestpassage.xlsx")
IS <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/IS_northwestpassage.xlsx")
NO <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/NO_northwestpassage.xlsx")
RU <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/RU_northwestpassage.xlsx")
SE <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/SE_northwestpassage.xlsx")
US <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/US_northwestpassage.xlsx")

DE <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/DE_northwestpassage.xlsx")
NL <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/NL_northwestpassage.xlsx")
#PL <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/PL_northwestpassage.xlsx")
GB <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/GB_northwestpassage.xlsx")
FR <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/FR_northwestpassage.xlsx")
ES <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/ES_northwestpassage.xlsx")
IT <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/IT_northwestpassage.xlsx")
#JP <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/JP_northwestpassage.xlsx")
#CN <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CN_northwestpassage.xlsx")
IN <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/IN_northwestpassage.xlsx")
KR <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/KR_northwestpassage.xlsx")
SG <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/SG_northwestpassage.xlsx")
CH <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CH_northwestpassage.xlsx")


NorthWestPassage <- rbind(CA,DK,FI,IS,NO,RU,SE,US,DE,NL,GB,FR,ES,IT,IN,KR,SG,CH)
write_xlsx(NorthWestPassage, "/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/aa_groupings/nwpassage.xlsx")

#####
#NortheastPassage
CA <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CA_northeastpassage.xlsx")
DK <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/DK_northeastpassage.xlsx")
FI <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/FI_northeastpassage.xlsx")
#IS <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/IS_northeastpassage.xlsx")
NO <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/NO_northeastpassage.xlsx")
#RU <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/RU_northeastpassage.xlsx")
SE <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/SE_northeastpassage.xlsx")
US <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/US_northeastpassage.xlsx")

DE <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/DE_northeastpassage.xlsx")
NL <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/NL_northeastpassage.xlsx")
PL <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/PL_northeastpassage.xlsx")
GB <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/GB_northeastpassage.xlsx")
FR <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/FR_northeastpassage.xlsx")
ES <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/ES_northeastpassage.xlsx")
IT <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/IT_northeastpassage.xlsx")
JP <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/JP_northeastpassage.xlsx")
CN <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CN_northeastpassage.xlsx")
IN <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/IN_northeastpassage.xlsx")
KR <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/KR_northeastpassage.xlsx")
SG <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/SG_northeastpassage.xlsx")
CH <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CH_northeastpassage.xlsx")



NEPassage <- rbind(CA, DK, FI, NO, SE, US, DE, NL, PL, GB, FR,ES, IT, JP, CN, IN, KR, SG, CH)
write_xlsx(NEPassage, "/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/aa_groupings/nepassage.xlsx")



#Northeast Passage
CA <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CA_northeastpassage.xlsx")
DK <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/DK_northeastpassage.xlsx")
FI <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/FI_northeastpassage.xlsx")
#IS <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/IS_northeastpassage.xlsx")
NO <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/NO_northeastpassage.xlsx")
#RU <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/RU_northeastpassage.xlsx")
SE <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/SE_northeastpassage.xlsx")
US <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/US_northeastpassage.xlsx")

#DE <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/DE_northeastpassage.xlsx")
NL <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/NL_northeastpassage.xlsx")
PL <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/PL_northeastpassage.xlsx")
GB <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/GB_northeastpassage.xlsx")
#FR <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/FR_northeastpassage.xlsx")
#ES <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/ES_northeastpassage.xlsx")
#IT <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/IT_northeastpassage.xlsx")
#JP <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/JP_northeastpassage.xlsx")
#CN <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CN_northeastpassage.xlsx")
IN <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/IN_northeastpassage.xlsx")
#KR <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/KR_northeastpassage.xlsx")
#SG <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/SG_northeastpassage.xlsx")
#CH <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CH_northeastpassage.xlsx")


NorthEastPassage <- rbind(CA, DK, FI, GB, IN, NL, NO, PL, SE, US)
write_xlsx(NorthEastPassage, "/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/aa_groupings/northeastpassage.xlsx")

#Arctic Passage
CA <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CA_arcticpassage.xlsx")
DK <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/DK_arcticpassage.xlsx")
FI <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/FI_arcticpassage.xlsx")
IS <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/IS_arcticpassage.xlsx")
NO <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/NO_arcticpassage.xlsx")
RU <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/RU_arcticpassage.xlsx")
SE <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/SE_arcticpassage.xlsx")
US <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/US_arcticpassage.xlsx")

DE <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/DE_arcticpassage.xlsx")
NL <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/NL_arcticpassage.xlsx")
PL <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/PL_arcticpassage.xlsx")
GB <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/GB_arcticpassage.xlsx")
FR <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/FR_arcticpassage.xlsx")
ES <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/ES_arcticpassage.xlsx")
IT <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/IT_arcticpassage.xlsx")
#JP <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/JP_arcticpassage.xlsx")
CN <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CN_arcticpassage.xlsx")
IN <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/IN_arcticpassage.xlsx")
#KR <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/KR_arcticpassage.xlsx")
SG <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/SG_arcticpassage.xlsx")
CH <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CH_arcticpassage.xlsx")


ArcticPassage <- rbind(CA, DK, FI, IS, NO, RU, SE, US, DE, NL, PL, GB, FR, ES, IT, CN, IN, SG, CH)
write_xlsx(ArcticPassage, "/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/aa_groupings/arcticpassage.xlsx")

#Arctic Icebreaker
CA <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CA_arcticicebreaker.xlsx")
DK <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/DK_arcticicebreaker.xlsx")
FI <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/FI_arcticicebreaker.xlsx")
IS <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/IS_arcticicebreaker.xlsx")
NO <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/NO_arcticicebreaker.xlsx")
RU <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/RU_arcticicebreaker.xlsx")
SE <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/SE_arcticicebreaker.xlsx")
US <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/US_arcticicebreaker.xlsx")

DE <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/DE_arcticicebreaker.xlsx")
NL <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/NL_arcticicebreaker.xlsx")
PL <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/PL_arcticicebreaker.xlsx")
GB <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/GB_arcticicebreaker.xlsx")
FR <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/FR_arcticicebreaker.xlsx")
ES <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/ES_arcticicebreaker.xlsx")
IT <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/IT_arcticicebreaker.xlsx")
JP <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/JP_arcticicebreaker.xlsx")
#CN <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CN_arcticicebreaker.xlsx")
IN <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/IN_arcticicebreaker.xlsx")
#KR <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/KR_arcticicebreaker.xlsx")
SG <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/SG_arcticicebreaker.xlsx")
CH <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/CH_arcticicebreaker.xlsx")


ArcticIcebreaker <- rbind(CA, DK, FI, IS, NO, RU, SE, US, DE, NL, PL, GB, FR, ES, IT, JP, IN, SG, CH)
write_xlsx(ArcticIcebreaker, "/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/aa_groupings/arcticicebreaker.xlsx")

#ALL_Merge
ArcticIcebreaker <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/aa_groupings/arcticicebreaker.xlsx")
ArcticPassage <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/aa_groupings/arcticpassage.xlsx")
#NorthEastPassage <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/aa_groupings/northeastpassage.xlsx")
NorthWestPassage <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/aa_groupings/nwpassage.xlsx")
ArcticCouncil <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/aa_groupings/arcticcouncil.xlsx")
#ArcticTrade <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/aa_groupings/arctictrade.xlsx")
ArcticShipping <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/aa_groupings/arcticshipping.xlsx")


ALLFINALTweets_Arctic <- rbind(ArcticShipping, ArcticCouncil, NorthWestPassage, ArcticPassage, ArcticIcebreaker)
write_xlsx(ALLFINALTweets_Arctic, "/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/aa_groupings/ALLFINALTweets_arctic.xlsx")


ALL <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/aa_groupings/ALLFinal_tweets.xlsx")
FinalTweets <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/aa_groupings/ALLFINALTweets_arctic.xlsx")
