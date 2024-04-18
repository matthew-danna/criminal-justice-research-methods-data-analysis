# Step 1: packages
# install these. One time only!
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

# Step 2
setwd("C:/Users/mdanna2/Downloads/research articles") #Windows

# Step 3
my.data <- data.frame()
file.list <- list.files(pattern = "*.pdf", recursive = TRUE)

for (file in file.list) {
  tmp.data <- data.frame(pdf_text(file))
  tmp.data$file <- file
  names(tmp.data) <- c("text", "file")
  my.data <- rbind(tmp.data, my.data)
}

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

stop_words$word.ID <- seq.int(nrow(stop_words))
stops.stemmed <- stem_words(stop_words$word)
stops.stemmed <- as.data.frame(unlist(stops.stemmed))
stops.stemmed$word.ID <- seq.int(nrow(stops.stemmed))
stops.stemmed$lexicon <- "Stemmed Stopword"
names(stops.stemmed) <- c("word", "word.ID", "lexicon")
all.stops <- rbind(stop_words, stops.stemmed)

no.stop.words <- my.stem.words %>% anti_join(all.stops, by = "word")
names(no.stop.words) <- c("file", "file.ID", "word", "word.ID", "stem.word")

tmp.clean.words <- no.stop.words[grep("^[[:digit:]]", no.stop.words$word), ]
my.clean.words <- no.stop.words %>% anti_join(tmp.clean.words, by = "word")

# Step 4
## sentiment
my.afinn <- my.clean.words %>% inner_join(get_sentiments("afinn"))
my.bing <- my.clean.words %>% inner_join(get_sentiments("bing"))
my.loughran <- my.clean.words %>% inner_join(get_sentiments("loughran"))
my.nrc <- my.clean.words %>% inner_join(get_sentiments("nrc"))

my.sentiment <- my.bing

## word counts
counts.my.clean.words <- my.clean.words %>% count(word, sort = TRUE)
counts.my.sentiment <- my.sentiment %>% count(word, sort = TRUE)
counts.sentiments <- my.sentiment %>% count(sentiment, sort = TRUE)
counts.words.sentiments <- my.sentiment %>% count(word, sentiment, sort = TRUE)

counts.koper1995 <- my.sentiment %>% 
  subset(file == "Koper 1995.pdf") %>% 
  count(word, sentiment, sort = TRUE)

top.my.clean.words <- subset(counts.my.clean.words, 
                             n >= quantile(counts.my.clean.words$n, 0.999))
## word pairs
wordpairs.my.words <- my.clean.words %>% 
  pairwise_count(word, file.ID, sort = TRUE)

wordpairs.koper1995 <- my.clean.words %>% 
  subset(file == 'Koper 1995.pdf') %>% 
  pairwise_count(word, file.ID, sort = TRUE)

wordpairs.top <- subset(wordpairs.my.words, 
                        n >= quantile(wordpairs.my.words$n, 0.99999))

# Step 5: graphs!
top.my.clean.words.sentiment <- subset(counts.words.sentiments, 
                                       n >= quantile(counts.words.sentiments$n, 0.99))
ggplot(top.my.clean.words.sentiment, aes(x= reorder(word, n), y=n, fill= sentiment)) + 
  geom_bar(stat = "identity") + 
  coord_flip()

set.seed(611)
pairs.plot <- wordpairs.my.words %>%
  filter(n >= 500) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "steelblue") +
  ggtitle("Word Pairs!") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
pairs.plot

my.clean.article <- subset(my.clean.words, file == "d_anna 2016.pdf") #yep we’re testing mine
counts.my.clean.article <- my.clean.article %>% count(word, sort = TRUE)
top.my.clean.article <- subset(counts.my.clean.article, n >= quantile(counts.my.clean.article$n, 0.95))
wordpairs.article <- my.clean.article %>% pairwise_count(word, file.ID, sort = TRUE)
ggplot(top.my.clean.article, aes(x=word, y=n, fill=n)) + geom_bar(stat = "identity") + coord_flip()
set.seed(611)
pairs.plot.article <- wordpairs.article %>%
  filter(n >= 5)               %>%
  graph_from_data_frame()        %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "steelblue") +
  ggtitle("Word Pairs for one article") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
pairs.plot.article

### Topic modeling!
my.clean.words <- my.clean.words %>%
  mutate(Sentiment = map_int(my.clean.words$word,happyorsad,"da"))

words_topic <- my.clean.words             %>%
  count(file, word, sort = TRUE) %>%
  ungroup()
reviewDTM <- words_topic       %>%
  cast_dtm(file, word, n)

reviewLDA <- LDA(reviewDTM, k = 10, control = list(seed = 347)) # experiment with ‘10’ as the “right” number of topics

topics <- tidy(reviewLDA, matrix = "beta")

topTerms <- topics %>%
  group_by(topic)          %>%
  top_n(9, beta)           %>% # change this accordingly for more or less words
  ungroup()                %>%
  arrange(topic, -beta)    %>%
  mutate(order = rev(row_number()))

plot_topics <- topTerms %>%
  ggplot(aes(order, beta)) +
  ggtitle("topics") +
  geom_col(show.legend = FALSE, fill = "steelblue") +
  scale_x_continuous(
    breaks = topTerms$order,
    labels = topTerms$term,
    expand = c(0,0)) +
  facet_wrap(~ topic,scales = "free") +
  coord_flip(ylim = c(0,0.02)) +
  theme(axis.title = element_blank())
plot_topics


