
#DATA ANALYSIS AND MERGING

#####
#Load Packages
#####
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
library(tidyverse)
library(tidytext)

#resets the ENV
rm(list=ls())

#####
#ADD DATA TO df and clean
#####
news <- read_excel(
  "/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/cleaned/News_ALL.xlsx")
pubs <- read_excel(
  "/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/cleaned/Publications_ALL.xlsx")
tweets <- read_excel(
  "/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/cleaned/Socials_ALL_FINAL.xlsx")
#####
tweets$lang <- "en"
tweets2 <-bind_cols(tweets$created_at, tweets$author_id, tweets$org_text, tweets$new_text, tweets$source, tweets$lang, tweets$possibly_sensitive,
                    tweets$Country, tweets$year, tweets$query, tweets$status)
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
#####

#Decide which data type you want to work with
observations <- tweets

#ensuring all data files are unique (no duplicates)
final_obs <- distinct(observations)
#transferring into a list format
dfList<-list(final_obs)

#create a function to do all the data cleaning processes
result_list <- llply(dfList, function(x) {
  x<-subset(x,x$`lang`== "en") 
  x$alpha=x$`new_text` 
  x<-x[,4] 
  x$obs_count<-1:length(x$new_text) 
  return(x)
})


#apply the function to each dataset
twts1<-as.data.frame(result_list[1])

#####
#N-GRAM ANALYSIS - Word Frequency
#####
#stop_words is a combination of English stop words from three lexicons, as a data frame. 
data(stop_words)

#customize stop words
custom_stop_words <- bind_rows(
  tibble(word = c("arctic", "council", "shipping", "northwest", "icebreaker", "passage"), 
         lexicon = c("custom")), stop_words)

#store those special symbols in the variable so we can remove them later
remove_reg <- "&amp;|&lt;|&gt;"
ompleterecords <- na.omit(twts1)
twts1 <- ompleterecords

#create a list containing all six data frames
twts1[complete.cases(twts1), ]

dfList2 <- list(twts1)
result_list2 <- 
  llply(dfList2, function(x) {
    y <- x %>% 
      mutate(tweet = str_remove_all(new_text, remove_reg)) %>%
      unnest_tokens(word, new_text, token = "tweets") %>%
      filter(!word %in% custom_stop_words$word,
             str_detect(word, "[a-z]"))
    return(y)})

df1<-as.data.frame(result_list2)


tweets2 <-bind_cols(tweets$created_at, tweets$author_id, tweets$org_text, tweets$new_text, tweets$source, tweets$lang, tweets$possibly_sensitive,
                    tweets$Country, tweets$year, tweets$query, tweets$status)
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

#Count the Frequency for Each Word
tidy_ngram_analysis <- tidy1 %>%dplyr::count(word, sort = TRUE) 

#Remove all non-english tokens
tidy_ngram_analysis2 <- tidy_ngram_analysis[which(!grepl("[^\x01-\x7F]+", tidy_ngram_analysis$word)),]

dfList3<-list(tidy_ngram_analysis2)

result_list3 <- 
  llply(dfList3, function(x) {
    plot <- x %>%
      #keep only the top 20 tokens
      dplyr::top_n(50) %>%
      #reorder word based on the count
      dplyr::mutate(word = reorder(word, n)) %>%
      #plot using ggplot2
      ggplot(aes(word, n, fill=word)) +
      #specify it's a bar plot
      geom_bar(stat="identity")+
      scale_fill_hue(c=45, l=80)+
      xlab(NULL) +
      coord_flip()+
      theme(legend.position="none")
    return(plot)})

#plot of top x terms
result_list3[[1]]


#####
#BI-GRAM ANALYSIS - Doublez-vous terms list
#####
#Archive
tidy_bigram_analysis <- unnest_tokens(tidy1, word, tweet, token = "ngrams", n = 2)

tidy_bigram_analysis2 <- tidy_bigram_analysis[which(!grepl("[^\x01-\x7F]+", tidy_bigram_analysis$word)),]

dfList4<-list(tidy_bigram_analysis2)

whyme <- as.data.frame(dfList4)

head(whyme[, c("obs_count", "word")])
gimmie <- whyme %>% count(word, sort=TRUE)
doublez_vous_table <- as.data.frame(gimmie)
#####

data(stop_words)

#customize stop words
custom_stop_words <- bind_rows(
  tibble(word = c("arctic", "council", "shipping", "northwest", "icebreaker", "passage"), 
         lexicon = c("custom")), stop_words)


# Function to remove stopwords from a text column
remove_stopwords <- function(data, text_column, custom_stopwords = character(0)) {
  cleaned_data <- data %>%
    anti_join(stop_words, by = c(text_column = "word"))
  
  return(cleaned_data)
}

# Example usage to remove stopwords from 'new_text'
# Assuming your raw data frame is named 'your_data' and the text column is 'new_text'
cleaned_data <- remove_stopwords(pubs, "new_text")

# View the cleaned data
head(cleaned_data)










count_ngrams <- function(data, text_column, n) {
  # Tokenize and count n-grams
  ngram_counts <- data %>%
    unnest_tokens(ngram, text_column, token = "ngrams", n = n) %>%
    count(ngram, sort = TRUE)
  
  # Calculate summed value and proportion
  total_unique_ngrams <- nrow(ngram_counts)
  
  ngram_counts <- ngram_counts %>%
    mutate(
      summed_value = sum(n),
      proportion = n / total_unique_ngrams
    )
  
  return(ngram_counts)
}

# Example usages: Assuming your raw data frame is named 'your_data' and the text column is 'new_text'
# N-Gram
pubs_ngram_counts <- count_ngrams(pubs, "new_text", 1)
tweets_ngram_counts <- count_ngrams(tweets, "new_text", 1)
news_ngram_counts <- count_ngrams(news, "new_text", 1)

# Bi-Gram
pubs_bigram_counts <- count_ngrams(pubs, "new_text", 2)
tweets_bigram_counts <- count_ngrams(tweets, "new_text", 2)
news_bigram_counts <- count_ngrams(news, "new_text", 2)

#To data frame and export
#Ngram
pub_word_count <- as.data.frame(pubs_ngram_counts)
write_xlsx(pub_word_count,
           "/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/FINAL/pubs_ngram.xlsx")
SM_word_count <- as.data.frame(tweets_ngram_counts)
write_xlsx(SM_word_count,
           "/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/FINAL/SM_ngram.xlsx")
news_word_count <- as.data.frame(news_ngram_counts)
write_xlsx(news_word_count,
           "/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/FINAL/news_ngram.xlsx")

#Bigram
pub_bi_count <- as.data.frame(pubs_bigram_counts)
write_xlsx(pub_bi_count,
           "/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/FINAL/pubs_bigram.xlsx")
SM_bi_count <- as.data.frame(tweets_bigram_counts)
write_xlsx(SM_bi_count,
           "/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/FINAL/SM_bigram.xlsx")
news_bi_count <- as.data.frame(news_bigram_counts)
write_xlsx(news_bi_count,
           "/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/FINAL/news_bigram.xlsx")



#save as excel file

#####
#WORD CORRELATION 
#####
dfList2<-list(tidy1)

result_list10 <- 
  llply(dfList2, function(x) {
    pair <- x %>%
      #for news and publications use 'new_text', for socials use 'tweet' as the second word in the list
      unnest_tokens(word, tweet) %>%
      filter(!word %in% custom_stop_words$word)%>%
      group_by(word) %>%
      #mentioned x amount of times
      filter(n() >= 5) %>%
      pairwise_cor(word, obs_count, sort = TRUE)%>%
      #term being correlated
      filter(item1 %in% c("just")) %>%
      group_by(item1) %>%
      #Top x amount of terms
      top_n(10) %>%
      ungroup() %>%
      mutate(item2 = reorder(item2, correlation))
    return(pair)})

#Run the results
result_list10[[1]]
#conversion to dataframe and renaming columns
word_correlation_table <- as.data.frame(result_list10)

library(widyr)

colnames(word_correlation_table)[1] <- "selected_term"
colnames(word_correlation_table)[2] <- "correlated_term"
colnames(word_correlation_table)[3] <- "correlated_pct"

write_xlsx(word_correlation_table,
           "/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/cleaned/00_final/Socials_correlated_just.xlsx")

#####
#SAVE ALL SPREADSHEETS
#####

#Clean data
write_xlsx(twts1,
           "/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/cleaned/00_final/Socials_GramCor_Prepped.xlsx")
twts1 <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/cleaned/00_final/Socials_GramCor_Prepped.xlsx")

#N-Gram
write_xlsx(tidy_ngram_analysis2,
           "/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/cleaned/00_final/Socials_ngram.xlsx")
tidy_ngram_analysis2 <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/cleaned/00_final/Socials_ngram.xlsx")


#Bi-gram
write_xlsx(doublez_vous_table,
           "/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/cleaned/00_final/Socials_bigram.xlsx")
doublez_vous_table <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/cleaned/00_final/Socials_bigram.xlsx")

#Word-correlation
write_xlsx(word_correlation_table,
           "/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/cleaned/00_final/News_correlated_term6.xlsx")
word_correlation_table <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/cleaned/00_final/Socials_correlated_term1.xlsx")


