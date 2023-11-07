### Step 1: LIBRARIES
install.packages('readtext')
install.packages('pdftools')
install.packages('tidyverse')
install.packages('tidytext')
install.packages('textstem')
install.packages('textdata')
install.packages('widyr')
install.packages('tm')
install.packages('reshape2')
install.packages('igraph')
install.packages('ggraph')
install.packages('wordcloud')
install.packages('topicmodels')
install.packages('devtools')
devtools::install_github("56north/happyorsad")
devtools::install_github("brooke-watson/BRRR")

library(readtext)
library(pdftools)
library(tidyverse)
library(tidytext)
library(textstem)
library(textdata)
library(widyr)
library(tm)
library(reshape2)
library(igraph)
library(ggraph)
library(wordcloud)
library(topicmodels)
library(happyorsad)
library(BRRR)

### Step 2: DATA
setwd("C:/Users/USERNAME HERE/Desktop/research articles") #Windows
setwd("/Users/USERNAME HERE/Desktop/research articles") #Mac
setwd("/Users/matthewdanna/Downloads/research articles") #Mac
skrrrahh(0)

### Step 3: CLEAN DATA
my.data <- data.frame()
file.list <- list.files(pattern = "*.pdf", recursive = TRUE)

for (file in file.list) {
  tmp.data <- data.frame(pdf_text(file))
  tmp.data$file <- file
  names(tmp.data) <- c("text", "file")
  my.data <- rbind(tmp.data, my.data)
}

# STEMMING WORDS
my.data$file.ID <- seq.int(nrow(my.data))
my.data$text <- as.character(my.data$text)
my.words <- my.data %>% unnest_tokens(word, text)
my.words$word.ID <- seq.int(nrow(my.words))
my.stems <- stem_words(my.words$word)
my.stems <- as.data.frame(unlist(my.stems))
my.stems$word.ID <- seq.int(nrow(my.stems))
names(my.stems) <- c("stem.word", "word.ID")
my.stem.words <- my.words %>% inner_join(my.stems)

names(my.stem.words) <- c("file", "file.ID", "original.word", "word.ID", "word")

# STOP WORDS
stop_words$word.ID <- seq.int(nrow(stop_words))
stops.stemmed <- stem_words(stop_words$word)
stops.stemmed <- as.data.frame(unlist(stops.stemmed))
stops.stemmed$word.ID <- seq.int(nrow(stops.stemmed))
stops.stemmed$lexicon <- "Stemmed Stopword"
names(stops.stemmed) <- c("word", "word.ID", "lexicon")
all.stops <- rbind(stop_words, stops.stemmed)

no.stop.words <- my.stem.words %>% anti_join(all.stops, by = "word")
names(no.stop.words) <- c("file", "file.ID", "word", "word.ID", "stem.word")

# REMOVING NUMBERS, LEAVING USABLE WORDS LEFT
tmp.clean.words <- no.stop.words[grep("^[[:digit:]]", no.stop.words$word), ]
my.clean.words <- no.stop.words %>% anti_join(tmp.clean.words, by = "word")

### Step 4: ANALYZE DATA
## Sentiment
my.afinn <- my.clean.words %>% inner_join(get_sentiments("afinn"))
my.bing <- my.clean.words %>% inner_join(get_sentiments("bing"))
my.loughran <- my.clean.words %>% inner_join(get_sentiments("loughran"))
my.nrc <- my.clean.words %>% inner_join(get_sentiments("nrc"))

summary.nrc <- my.nrc %>%
  group_by(sentiment) %>%
  summarise(count = n()) %>%
  mutate(PCT = round(count/sum(count)*100,2))

my.sentiment <- my.afinn

## Word Counts
# counts of unique usable words
counts.my.clean.words <- my.clean.words %>% 
  count(word, sort = TRUE) %>%
  mutate(PCT = round(n/sum(n)*100,2))

# counts of words with sentiment
counts.my.sentiment <- my.sentiment %>% 
  count(word, sort = TRUE) %>%
  mutate(PCT = round(n/sum(n)*100,2))

counts.sentiments <- my.sentiment %>% count(sentiment, sort = TRUE)


