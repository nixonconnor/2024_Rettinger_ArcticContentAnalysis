

#DATA CLEANING SCRIPT - NEWS

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

news <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/raw/NewsArticles_ALL.xlsx")
news$lang <- "en"
news$possibly_sensitive <- ""


news <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/cleaned/News_ALL.xlsx")

#keeping only relevant twitter data
new_news <- bind_cols(news$publish_date, news$primary_author, news$article_text, news$source, news$lang, news$possibly_sensitive)
colnames(new_news)[1] <- "created_at"
colnames(new_news)[2] <- "author_id"
colnames(new_news)[3] <- "text"
colnames(new_news)[4] <- "source"
colnames(new_news)[5] <- "lang"
colnames(new_news)[6] <- "possibly_sensitive"

#save the new dataset
write_xlsx(new_news,
           "/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/cleaned/News/News_Cleaned1.xlsx")


##### STEP 5: Moving data to 'twitterCorpus' file to clean dataframe $text
#```{r}
twitterCorpus <-Corpus(VectorSource(new_news$text))
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
new_news2 <- bind_cols(news$publish_date, news$primary_author, news$article_text, dataframe$text, news$source, news$lang, news$possibly_sensitive)
new_news2$Country <- ""
new_news2$year <- news$year
new_news2$query <- news$query
new_news2$status <- ""

colnames(new_news2)[1] <- "created_at"
colnames(new_news2)[2] <- "author_id"
colnames(new_news2)[3] <- "org_text"
colnames(new_news2)[4] <- "new_text"
colnames(new_news2)[5] <- "source"
colnames(new_news2)[6] <- "lang"
colnames(new_news2)[7] <- "possibly_sensitive"
colnames(new_news2)[8] <- "Country"
colnames(new_news2)[9] <- "year"
colnames(new_news2)[10] <- "query"
colnames(new_news2)[11] <- "status"

#Org_text limitation fxn to ensure it fits within excel guidelines of 32,767 character limit
limit <- function(new_news2, org_text) {
  new_news2 %>%
    mutate(across({{org_text}}, ~substr(.x, 1, 32765)))
}

# Apply the function to limit characters in the "Text" column
df_limited <- limit(new_news2, org_text)

# View the resulting data frame
print(df_limited)

#SAVE ANALYSIS
write_xlsx(df_limited,
           "/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/cleaned/News_ALL.xlsx")

write.csv(new_news2,
          "/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/cleaned/News_ALL.csv")
