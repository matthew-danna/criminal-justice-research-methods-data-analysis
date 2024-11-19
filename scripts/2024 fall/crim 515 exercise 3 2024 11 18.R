# Step 0
install.packages('pdftools')
install.packages('tidytext')
install.packages('tidyverse')
install.packages('textstem')
install.packages('widyr')
install.packages('ggraph')
install.packages('igraph')

library(pdftools)
library(tidytext)
library(tidyverse)
library(textstem)
library(widyr)
library(ggraph)
library(igraph)

# Step 1
### update for your file path and article name
my.data <- data.frame(pdf_text("C:/Users/USER NAME/Downloads/research articles/ARTICLE NAME.pdf"))

# Step 2
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

no.stops <- my.stem.words %>% anti_join(stop_words, by = 'word')
no.words <- no.stops[grep("^[[:digit:]]", 
                          no.stops$word), ]
my.clean.words <- no.stops %>% anti_join(no.words, by = 'word')

# Step 3 (uses the stemmed word)
wordpairs <- my.clean.words %>% pairwise_count(word, page, sort = TRUE)

set.seed(1005)
pairs.plot <- wordpairs %>%
  filter(n >= 13) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "steelblue") +
### update with a better title
  ggtitle("Top Word Co-Occurrences for the article I picked") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
pairs.plot

# Step 3.5 (uses the full, original word)
wordpairs <- my.clean.words %>% pairwise_count(original.word, page, sort = TRUE)

set.seed(1005)
pairs.plot <- wordpairs %>%
  filter(n >= 12) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "steelblue") +
### update with a better title  
  ggtitle("Top Word Co-Occurrences for my article") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
pairs.plot


