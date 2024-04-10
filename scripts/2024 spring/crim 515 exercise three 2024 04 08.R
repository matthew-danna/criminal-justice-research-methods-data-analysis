# step 0: libraries
install.packages('pdftools')
install.packages('tidytext')
install.packages('tidyverse')
install.packages('textstem')
install.packages('wordcloud')
install.packages('wordcloud2')
install.packages('tm')
library(pdftools)
library(tidytext)
library(tidyverse)
library(textstem)
library(wordcloud)
library(wordcloud2)
library(tm)

# Step 1: get data
### from the internet:
my.data <- data.frame(pdf_text("https://github.com/matthew-danna/justice-research/raw/main/article.pdf"))

### from your computer;
my.data2 <- data.frame(pdf_text("C:/Users/mdanna2/Downloads/article.pdf"))

# Step 2: clean data
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

# Step 3: wordclouds
### basic:
wordcloud(my.clean.words$original.word)
wordcloud(my.clean.words$word)

### less basic:
counts.word <- my.clean.words %>% count(word, sort = TRUE)
counts.word <- my.clean.words %>% count(original.word, sort = TRUE)
names(counts.word) <- c("word", "freq")
wordcloud2(data = counts.word, size = 1.6, 
           color = 'random-dark')
wordcloud2(data = counts.word, size = 0.8, 
           shape = 'pentagon')

