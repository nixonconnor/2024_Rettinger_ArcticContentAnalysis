
#DATA CLEANING SCRIPT - TWITTER

#Load Packages
library(writexl)
library(readxl)
library(tidytext)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(purrr)
library(textdata)
library(wordcloud)
library(reshape2)
library(stringr)
library(igraph)
library(ggraph)
library(widyr)
library(grid)
library(arules)
library(tm)
library(topicmodels)

#Double Check these are in there
packages <- c("readxl", "write_xl","tidytext","plyr","dplyr","tidyr","ggplot2","scales",
              "purrr","textdata","wordcloud","reshape2","stringr","igraph",
              "ggraph","widyr","grid","arules","tm","topicmodels")
for(i in packages){
  if(!require(i,character.only = T, quietly = T)){
    install.packages(i)
  }
  library(i, character.only = T, quietly = T)
}



#extracting tweets from twitter.com (or 'X')
ug.twitter <-
  get_all_tweets(
    query = "arctic icebreaker",
    start_tweets = "2007-01-01T00:00:00Z",
    end_tweets = "2022-12-31T00:00:00Z",
    data_path = "/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/raw/CH/arcticicebreaker.tweet",
    bind_tweets = FALSE,
    bearer_token = "AAAAAAAAAAAAAAAAAAAAAG6ijQEAAAAA5K8KAJqnBUFykDqLXZc%2BYPMfZ3Q%3Dv8t6lSN0hx3IxfWlU2Bxs7BhdzL5LyleplHtzTh7zn52KkMbgk",
    country = "CH",
    lang = "en",
    n=Inf )

tweets <- bind_tweets(
  data_path = "/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/raw/CH/arcticicebreaker.tweet")


#tweets <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/cleaned/Twitter/CA_arcticshipping.xlsx")
#tweets <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/cleaned/Twitter/CA_arctictrade.xlsx")

#keeping only relevant twitter data
tweets <- bind_cols(tweets$created_at, tweets$author_id, tweets$text, tweets$source, tweets$lang, tweets$possibly_sensitive)
colnames(tweets)[1] <- "created_at"
colnames(tweets)[2] <- "author_id"
colnames(tweets)[3] <- "text"
colnames(tweets)[4] <- "source"
colnames(tweets)[5] <- "lang"
colnames(tweets)[6] <- "possibly_sensitive"

#save the new dataset
write_xlsx(tweets,
           "/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/cleaned/Twitter/CH_arcticicebreaker.xlsx")


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


#Replace clean text with old text
tweets <- bind_cols(tweets$created_at, tweets$author_id, tweets$text, dataframe$text, tweets$source, tweets$lang, tweets$possibly_sensitive)
colnames(tweets)[1] <- "created_at"
colnames(tweets)[2] <- "author_id"
colnames(tweets)[3] <- "org_text"
colnames(tweets)[4] <- "new_text"
colnames(tweets)[5] <- "source"
colnames(tweets)[6] <- "lang"
colnames(tweets)[7] <- "possibly_sensitive"

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
#final_tweets$Country <- "Switzerland"


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
#final_tweets$query <- "arctic icebreaker"
#final_tweets$query <- "northeast passage"
#final_tweets$query <- "NW passage"
#final_tweets$query <- "arctic trade"

#Add membership status as column
#final_tweets$status <- "official member"
#final_tweets$status <- "observer state"

tweets$lang <- "en"
tweets2 <-bind_cols(tweets$created_at, tweets$author_id, tweets$org_text, tweets$new_text, tweets$source, tweets$lang, tweets$sensitive,
                    tweets$country, tweets$year, tweets$query, tweets$status)
colnames(tweets2)[1] <- "created_at"
colnames(tweets2)[2] <- "author_id"
colnames(tweets2)[3] <- "org_text"
colnames(tweets2)[4] <- "new_text"
colnames(tweets2)[5] <- "source"
colnames(tweets2)[6] <- "lang"
colnames(tweets2)[7] <- "possibly_sensitive"
colnames(tweets2)[8] <- "Country"
colnames(tweets2)[9] <- "year"
colnames(tweets2)[10] <- "query"
colnames(tweets2)[11] <- "status"

#SAVE ANALYSIS
write_xlsx(final_tweets,
           "/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/cleaned/Twitter/x_data.xlsx")

