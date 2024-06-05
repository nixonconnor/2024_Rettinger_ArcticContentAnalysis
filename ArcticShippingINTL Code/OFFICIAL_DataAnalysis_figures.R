

library(ggExtra)

#time-series analysis
p1.1 <- ggplot(FinalTweets, aes(x=created_at, y=sentiment, col=year)) +
  geom_point(size = .25, alpha = .5)+
  geom_smooth(se= TRUE, model= "gam", level = .95,)+
  labs(x='Date (years)', y='Observation Sentiment')+
  theme_classic()

ggMarginal(p1.1, type="histogram")
p1.1

#time-series analysis
p1 <- ggplot(FinalTweets, aes(x=created_at, y=sentiment, col=status)) +
  geom_point(size = .25, alpha = .5)+
  geom_smooth(se= TRUE, model= "gam", level = .95,)+
  labs(x='Date (years)', y='Observation Sentiment')+
  theme_classic()

ggMarginal(p1, type="histogram")
p1


p2 <- ggplot(ArcticShipping, aes(x=created_at, y=sentiment, col=Country)) +
  geom_point(size = .25, alpha = .5)+
  geom_smooth(se= TRUE, model= "gam", level = .95, col= "blue")+
  labs(x='Date (years)', y='Observation Sentiment')+
  theme_classic()
p2

p3 <- ggplot(ArcticCouncil, aes(x=created_at, y=sentiment, col=Country)) +
  geom_point(size = .25, alpha = .5)+
  geom_smooth(se= TRUE, model= "gam", level = .95, col= "blue")+
  labs(x='Date (years)', y='Observation Sentiment')+
  theme_classic()
p3

p4 <- ggplot(NWPassage, aes(x=created_at, y=sentiment, col=Country)) +
  geom_point(size = .25, alpha = .5)+
  geom_smooth(se= TRUE, model= "gam", level = .95, col= "blue")+
  labs(x='Date (years)', y='Observation Sentiment')+
  theme_classic()
p4


#word frequency



packages <- c("readxl","tidytext","plyr","dplyr","tidyr","ggplot2","scales",
              "purrr","textdata","wordcloud","reshape2","stringr","igraph",
              "ggraph","widyr","grid","arules","tm","topicmodels")
for(i in packages){
  if(!require(i,character.only = T, quietly = T)){
    install.packages(i)
  }
  library(i, character.only = T, quietly = T)
}

rm(list=ls())

#Set the seed to ensure that we get the same random numbers every time
#The seed could be any number you choose
set.seed(2022)

#Upload data
final_tweets <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/twitter_academia_arcticshipping_study/2_data/cleaned/aa_groupings/ALLFinal_tweets.xlsx")
final_tweets <- ALLMember_Arctic
colnames(final_tweets)[4] <- "text"

final_tweets <- distinct(final_tweets)
dfList<-list(final_tweets)
twts1 <- dfList
#create a function to do all the data clean process
result_list <- llply(dfList, function(x) {
  x<-subset(x,x$`lang`== "en") 
  x$tweet=x$`new_text` 
  x<-x[,3] 
  x$tweetnumber<-1:length(x$new_text) 
  return(x)
})

#apply the function to each dataset
twts1<-as.data.frame(twts1[1])
twts2<-as.data.frame(result_list[1])
twts3<-as.data.frame(result_list[1])
twts4<-as.data.frame(result_list[1])

#stop_words is a combination of English stop words from three lexicons, as a data frame. 
data(stop_words)

#customize stop words
custom_stop_words <- bind_rows(
  tibble(word = c("t.co"), 
         lexicon = c("custom")), stop_words)

#store those special symbols in the variable so we can remove them later
remove_reg <- "&amp;|&lt;|&gt;"
ompleterecords <- na.omit(twts1)
twts1 <- ompleterecords

#create a list containing all six data frames
twts1[complete.cases(twts1), ]

dfList2<-list(twts1)
result_list2 <- 
  llply(dfList2, function(x) {
    y <- x %>% 
      #remove special symbols for the values under the tweet variable
      mutate(tweet = str_remove_all(new_text, remove_reg)) %>%
      #extract every word from every tweet 
      unnest_tokens(word, new_text, token = "tweets") %>%
      #filter out all stop words
      filter(!word %in% custom_stop_words$word,
             str_detect(word, "[a-z]"))
    return(y)})


tidy1<-as.data.frame(result_list2[1])
tidy2<-as.data.frame(result_list2[1])
tidy3<-as.data.frame(result_list2[1])
tidy4<-as.data.frame(result_list2[1])

#Count the Frequency for Each Word
tidy_week11 <- tidy1 %>%dplyr::count(word, sort = TRUE) 
tidy_week12 <- tidy2 %>%dplyr::count(word, sort = TRUE)
tidy_week21 <- tidy3 %>%dplyr::count(word, sort = TRUE)
tidy_week22 <- tidy4 %>%dplyr::count(word, sort = TRUE)

#Remove all non-english tokens
tidy1_english <- tidy_week11[which(!grepl("[^\x01-\x7F]+", tidy_week11$word)),]
tidy2_english <- tidy_week12[which(!grepl("[^\x01-\x7F]+", tidy_week12$word)),]
tidy3_english <- tidy_week21[which(!grepl("[^\x01-\x7F]+", tidy_week21$word)),]
tidy4_english <- tidy_week22[which(!grepl("[^\x01-\x7F]+", tidy_week22$word)),]

dfList3<-list(tidy1_english)
result_list3 <- 
  llply(dfList3, function(x) {
    plot <- x %>%
      #keep only the top 20 tokens
      dplyr::top_n(20) %>%
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

result_list3[[1]]
result_list3[[2]]
result_list3[[3]]

#create a list containing all six data frames (ADD tidy4)
dfList4<-list(tidy1)
result_list_contribute <- 
  llply(dfList4, function(x) {
    plot <- x %>%
      inner_join(get_sentiments("nrc")) %>%
      dplyr::count(word, sentiment, sort = TRUE)  %>%
      group_by(sentiment) %>%
      top_n(10) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free") +
      labs(y = "Contribution to sentiment",
           x = NULL) +
      coord_flip()+
      scale_fill_grey() +
      scale_colour_grey() +
      theme_gray()
    return(plot)})

result_list_contribute[[1]]




#Word Correlation
dfList2 <- list(twts1)
result_list10 <- 
  llply(dfList2, function(x) {
    pair <- x %>%
      unnest_tokens(word, new_text) %>%
      filter(!word %in% custom_stop_words$word)%>%
      group_by(word) %>%
      filter(n() >= 20) %>%
      pairwise_cor(word, tweetnumber, sort = TRUE)%>%
      filter(item1 %in% c("tree")) %>%
      group_by(item1) %>%
      top_n(15) %>%
      ungroup() %>%
      mutate(item2 = reorder(item2, correlation)) %>%
      ggplot(aes(item2, correlation,fill=correlation)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ item1, scales = "free") +
      coord_flip()
    return(pair)})







#WordFrequency

review_text <- paste(ALL$new_text, collapse=" ")
review_source <- VectorSource(review_text)
corpus <- Corpus(review_source)

#clean
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

#Make TDM
dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)

frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)
freq <- as.data.frame(dtm2)

DF <- as.data.frame(as.matrix(dtm2), stringsAsFactors=False)

