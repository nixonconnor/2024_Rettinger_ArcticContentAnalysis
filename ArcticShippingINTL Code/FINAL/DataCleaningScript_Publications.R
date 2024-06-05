

#DATA CLEANING SCRIPT - PUBLICATIONS

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

pubs <- read_excel("/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/raw/AcademicLiterature_ALL.xlsx")
pubs$lang <- "en"
pubs$possibly_sensitive <- ""

#keeping only relevant twitter data
new_pubs <- bind_cols(pubs$publish_date, pubs$primary_author, pubs$article_abstract, pubs$source_name, pubs$lang, pubs$possibly_sensitive)
colnames(new_pubs)[1] <- "created_at"
colnames(new_pubs)[2] <- "author_id"
colnames(new_pubs)[3] <- "text"
colnames(new_pubs)[4] <- "source"
colnames(new_pubs)[5] <- "lang"
colnames(new_pubs)[6] <- "possibly_sensitive"

#save the new dataset
write_xlsx(new_pubs,
           "/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/cleaned/Publications/Publications_Cleaned1.xlsx")


##### STEP 5: Moving data to 'twitterCorpus' file to clean dataframe $text
#```{r}
twitterCorpus <-Corpus(VectorSource(new_pubs$text))
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
new_pubs2 <- bind_cols(new_pubs$created_at, new_pubs$author_id, new_pubs$text, dataframe$text, new_pubs$source, new_pubs$lang, new_pubs$possibly_sensitive)
new_pubs2$Country <- ""
new_pubs2$year <- pubs$year
new_pubs2$query <- pubs$query
new_pubs2$status <- ""

colnames(new_pubs2)[1] <- "created_at"
colnames(new_pubs2)[2] <- "author_id"
colnames(new_pubs2)[3] <- "org_text"
colnames(new_pubs2)[4] <- "new_text"
colnames(new_pubs2)[5] <- "source"
colnames(new_pubs2)[6] <- "lang"
colnames(new_pubs2)[7] <- "possibly_sensitive"
colnames(new_pubs2)[8] <- "Country"
colnames(new_pubs2)[9] <- "year"
colnames(new_pubs2)[10] <- "query"
colnames(new_pubs2)[11] <- "status"

#Org_text limitation fxn to ensure it fits within excel guidelines of 32,767 character limit
limit <- function(new_pubs2, org_text) {
  new_pubs2 %>%
    mutate(across({{org_text}}, ~substr(.x, 1, 32765)))
}

# Apply the function to limit characters in the "Text" column
df_limited <- limit(new_pubs2, org_text)

# View the resulting data frame
print(df_limited)

#SAVE ANALYSIS
write_xlsx(new_pubs2,
           "/Users/connornixon/Desktop/uOttawa_MSc/1_Arctic Shipping Content Analysis/2_data/cleaned/Publications_ALL.xlsx")
