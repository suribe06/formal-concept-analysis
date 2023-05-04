#install.packages(c('glue', 'cowplot', 'magrittr', 'plotly', 'tidyverse', 'widyr', 'hms', 'lubridate', 'tidytext', 'tm', 'wordcloud', 'igraph', 'networkD3'))
# Data Wrangling and Visualization
library(glue)
library(cowplot)
library(magrittr)
library(plotly)
library(tidyverse)
library(widyr)
# Date & Time Manipulation.
library(hms)
library(lubridate) 
# Text Mining
library(tidytext)
library(tm)
library(wordcloud)
# Network Analysis
library(igraph)
# Network Visualization (D3.js)
library(networkD3)

#FILE_DIR <- dirname(rstudioapi::getSourceEditorContext()$path)
FILE_DIR <- getwd()
setwd(FILE_DIR)
PARENT_DIR <- file.path(FILE_DIR, "..")
arti <- paste0("articles/combined_articles.txt")
text <- readLines(file.path(getwd(), arti))
text_df <- tibble(line = 1:length(text), text = text)

#Tokenization
text_df %<>%
  unnest_tokens(input = text, output = word) %>%
  filter(!is.na(word))

#Most frequent tokens
text_df %>% 
  count(word, sort = TRUE) %>%
  head(n = 10)

suppressMessages(suppressWarnings(library(gridExtra)))
text_df %>%
  count(word, sort = TRUE) %>%
  filter(n > 7) %>%
  mutate(word = reorder(word, n)) %>%
  head(20) %>%
  ggplot(aes(x = word, y = n)) +
  theme_light() + 
  geom_col(fill = 'darkolivegreen4', alpha = 0.8) +
  xlab(NULL) +
  ylab("Frecuencia") +
  coord_flip() +
  ggtitle(label = ' Conteo de palabras') -> words_count
grid.arrange(words_count)

suppressMessages(suppressWarnings(library(wordcloud)))
par(mfrow = c(1,1), mar = c(1,1,1,1), mgp = c(0,0,0))
set.seed(123)
text_df %>%
  count(word, sort = TRUE) %>%
  with(wordcloud(words = word, freq = n, max.words = 20, colors = 'darkolivegreen4'))
title(main = "Word Cloud") 

#relative frequencies 
bind_rows(mutate(.data = text_df, author = "Articulos")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n)) %>%
  select(-n) %>%
  spread(author, proportion, fill = 0) -> frec

frec %<>% 
  select(word, Articulos)

#top 10 tokens
frec %>%
  filter(Articulos != 0) %>%
  arrange(desc(Articulos)) -> frec_comun
dim(frec_comun) 
head(frec_comun, n = 10)



#(top 10 tokens)
frec %>%
  filter(Articulos != 0) %>%
  arrange(desc(Articulos)) -> frec_comun
dim(frec_comun) 
head(frec_comun, n = 10)

#Sentiment Analysis
positive_words <- read.csv(file.path(getwd(), "positive-words.txt"), sep="") %>%
  mutate(sentiment = "Positivo")
colnames(positive_words)[1] <- "word"
negative_words <- read.csv(file.path(getwd(), "negative-words.txt"), sep="") %>%
  mutate(sentiment = "Negativo")
colnames(negative_words)[1] <- "word"
sentiment_words <- bind_rows(positive_words, negative_words)

#Count words by sentiment
sentiment_words %>%
  count(sentiment)

suppressMessages(suppressWarnings(library(RColorBrewer)))
text_df %>%
  inner_join(sentiment_words) %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  mutate(n = ifelse(sentiment == "Negativo", -n, n)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = brewer.pal(8,'Dark2')[c(2,5)]) +
  coord_flip() +
  labs(y = "Frecuencia", x = NULL, title = "Count per sentiment") +
  theme_minimal() -> count_per_sentiment

grid.arrange(count_per_sentiment)

suppressMessages(suppressWarnings(library(reshape2)))
par(mfrow = c(1,1), mar = c(1,1,1,1), mgp = c(0,0,0))
set.seed(123)
text_df %>%
  inner_join(sentiment_words) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = brewer.pal(8,'Dark2')[c(2,5)], max.words = 50, title.size = 1.5)
title(main = "Word Cloud by Sentiment")

#Bigrams
names(text) <- NULL 
text_df <- tibble(line = 1:length(text), text = text)
text_df %>%
  unnest_tokens(input = text, output = bigram, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) -> text_df_bi

#top 10 bigrams
text_df_bi %>%
  count(bigram, sort = TRUE) %>%
  head(n = 10)

text_df_bi %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!is.na(word1)) %>% 
  filter(!is.na(word2)) %>%
  count(word1, word2, sort = TRUE) %>%
  rename(weight = n) -> text_df_bi_counts
dim(text_df_bi_counts)
head(text_df_bi_counts, n = 10)

# Define a network (binary, undirected, weighted, simple) from the frequency (weight) of the bigrams 
suppressMessages(suppressWarnings(library(igraph)))
# with a threshold of 1, there is a GCC. Different threshold, gives different network
umbral <- 1 
g <- text_df_bi_counts %>%
  filter(weight >= umbral) %>%
  graph_from_data_frame(directed = FALSE)
g <- igraph::simplify(g)
graph_name <- file.path(PARENT_DIR, "Graphs", paste0("graph_bigrams.gml"))
write.graph(g, graph_name, format = "gml")
# Graph induced by the GCC
V(g)$cluster <- clusters(graph = g)$membership
gcc <- induced_subgraph(graph = g, vids = which(V(g)$cluster == which.max(clusters(graph = g)$csize)))
set.seed(123)

#Skipgrams
names(text) <- NULL 
text_df <- tibble(line = 1:length(text), text = text)
text_df %>%
  unnest_tokens( input = text, output = skipgram, token = "skip_ngrams", n = 2) %>%
  filter(!is.na(skipgram)) -> text_df_skip

suppressMessages(suppressWarnings(library(ngram)))
# count words in each skipgram
text_df_skip$num_words <- text_df_skip$skipgram %>% 
  map_int(.f = ~ wordcount(.x))

# remover unigramas
text_df_skip %<>% 
  filter(num_words == 2) %>% 
  select(-num_words)

text_df_skip %>%
  separate(skipgram, c("word1", "word2"), sep = " ") %>%
  filter(!is.na(word1)) %>% 
  filter(!is.na(word2)) %>%
  count(word1, word2, sort = TRUE) %>%
  rename(weight = n) -> text_df_skip_counts

dim(text_df_skip_counts) 
head(text_df_skip_counts, n = 10)

# Create network from bigrams and the weights
umbral <- 1
g <- text_df_skip_counts %>%
  filter(weight >= umbral) %>%
  graph_from_data_frame(directed = FALSE)
g <- igraph::simplify(g)
graph_name <- file.path(PARENT_DIR, "Graphs", paste0("graph_skipgrams.gml"))
write.graph(g, graph_name, format = "gml")
V(g)$cluster <- clusters(graph = g)$membership
gcc <- induced_subgraph(graph = g, vids = which(V(g)$cluster == which.max(clusters(graph = g)$csize)))
set.seed(123)