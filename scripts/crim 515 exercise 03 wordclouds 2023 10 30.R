### Step 0: packages
install.packages('pdftools')
install.packages('tidytext')
install.packages('tidyverse')
install.packages('textstem')
install.packages('wordcloud')
install.packages('wordcloud2')
install.packages('tm')

library(pdftools)
library(tidyverse)
library(tidytext)
library(textstem)
library(wordcloud)
library(wordcloud2)

### Step 1: get data
# in-class example:
my.data <- data.frame(pdf_text("https://github.com/matthew-danna/justice-research/raw/main/article.pdf")) #internet
# online PDF example:
my.data <- data.frame(pdf_text("https://www.bjs.gov/content/pub/pdf/p18.pdf"))

my.data <- data.frame(pdf_text("C:/Users/USERNAME HERE/Desktop/ARTICLE NAME.pdf")) #Windows
my.data <- data.frame(pdf_text("/Users/matthewdanna/Downloads/class article.pdf")) #Mac

### Step 2: clean data
my.data$ID <- seq.int(nrow(my.data))
names(my.data) <- c("text", "page")
my.data$text <- as.character(my.data$text)

my.words <- my.data %>% unnest_tokens(word, text)
my.words$word.ID <- seq.int(nrow(my.words))

my.stems <- stem_words(my.words$word)
my.stems <- as.data.frame(unlist(my.stems))
my.stems$word.ID <- seq.int(nrow(my.stems))
names(my.stems) <- c("stem.word", "word.ID")

my.stem.words <- my.words %>% inner_join(my.stems)
names(my.stem.words) <- c("page", "original.word", 
                          "word.ID", "word")

no.stops <- my.stem.words %>% 
  anti_join(stop_words, by = "word")
no.words <- no.stops[grep("^[[:digit:]]", 
                          no.stops$word), ]
my.clean.words <- no.stops %>% 
  anti_join(no.words, by = "word")

### Step 3: wordclouds

# basic
wordcloud(my.clean.words$original.word)
wordcloud(my.clean.words$word)

# less basic
counts.word <- my.clean.words %>% count(original.word, sort = TRUE)
counts.word <- my.clean.words %>% count(word, sort = TRUE)
names(counts.word) <- c("word", "freq")

wordcloud2(data = counts.word, size = 1.6, color = 'random-dark')
wordcloud2(data = counts.word, size = 1.0, color = rep_len(c("green","blue","red"), nrow(counts.word)), shape = circle)
wordcloud2(data = counts.word, size = 0.8, shape = 'circle')




                                            
