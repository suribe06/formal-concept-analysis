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

FILE_DIR <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(FILE_DIR)
PARENT_DIR <- file.path(FILE_DIR, "..")

stop_words <- tibble(word = unlist(c(read.table(file.path(getwd(), "stopwords.txt"), quote="\"", comment.char=""))), lexicon = "custom")
#n <- 88
#for (i in 0:n){
  #arti <- paste0("articles/article_", i, ".txt")
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
    ggtitle(label = ' Conteo de palabras') -> p1
    grid.arrange(p1)
    
  suppressMessages(suppressWarnings(library(wordcloud)))
  par(mfrow = c(1,1), mar = c(1,1,1,1), mgp = c(0,0,0))
  set.seed(123)
  text_df %>%
    count(word, sort = TRUE) %>%
    with(wordcloud(words = word, freq = n, max.words = 20, colors = 'darkolivegreen4'))
  title(main = "Word Cloud") 
  
  #relative frequencies (top 10 tokens)
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
    geom_col() +
    scale_fill_manual(values = brewer.pal(8,'Dark2')[c(2,5)]) +
    coord_flip(ylim = c(-10,10)) +
    labs(y = "Frecuencia",
         x = NULL,
         title = "Count per sentiment") +
    theme_minimal() -> p1
  
  grid.arrange(p1)
   
  suppressMessages(suppressWarnings(library(reshape2)))
  par(mfrow = c(1,1), mar = c(1,1,1,1), mgp = c(0,0,0))
  set.seed(123)
  text_df %>%
   inner_join(sentiment_words) %>%
   count(word, sentiment, sort = TRUE) %>%
   acast(word ~ sentiment, value.var = "n", fill = 0) %>%
   comparison.cloud(colors = brewer.pal(8,'Dark2')[c(2,5)], 
                    max.words = 50, title.size = 1.5)
  title(main = "Word Cloud by Sentiment")
  
  #Bigrams
  names(text) <- NULL 
  text_df <- tibble(line = 1:length(text), text = text)
  
  #tokenize in bigrams, in this case each token is a bigram
  text_df %>%
   unnest_tokens(input = text, output = bigram, token = "ngrams", n = 2) %>%
   filter(!is.na(bigram)) -> text_df_bi
  
  #top 10 bigrams
  text_df_bi %>%
   count(bigram, sort = TRUE) %>%
   head(n = 10)
   
  text_df_bi %>%
   separate(bigram, c("word1", "word2"), sep = " ") %>%
   filter(!word1 %in% stop_words$word) %>%
   filter(!word2 %in% stop_words$word) %>%
   filter(!is.na(word1)) %>% 
   filter(!is.na(word2)) %>%
   count(word1, word2, sort = TRUE) %>%
   rename(weight = n) -> text_df_bi_counts  # importante para la conformacion de la red!
  dim(text_df_bi_counts)
  head(text_df_bi_counts, n = 10)
  
  ##### definir una red a partir de la frecuencia (weight) de los bigramas
  # binaria, no dirigida, ponderada, simple
  suppressMessages(suppressWarnings(library(igraph)))
  umbral <- 3
  g <- text_df_bi_counts %>%
   filter(weight >= umbral) %>%
   graph_from_data_frame(directed = FALSE)
  set.seed(123)
  #png("filter_graph.png", width = 15, height = 15, units = "cm", res=400)
  #plot(g, layout = layout_with_graphopt(g, charge = 0.1), vertex.color = 1, vertex.frame.color = 1, vertex.size = 3, vertex.label.color = 'black', vertex.label.cex = 1, vertex.label.dist = 1, main = "Umbral = 3") 
  #write.graph(g, "filter_graph.gml", format = "gml")
  #dev.off()
  
  # Giant Connected Component
  umbral <- 1
  g <- text_df_bi_counts %>%
    filter(weight >= umbral) %>%
    graph_from_data_frame(directed = FALSE)
  # grafo inducido por la componente conexa
  V(g)$cluster <- clusters(graph = g)$membership
  gcc <- induced_subgraph(graph = g, vids = which(V(g)$cluster == which.max(clusters(graph = g)$csize)))
  set.seed(123)
  
  # png("GCC_article.png", width = 15, height = 15, units = "cm", res=400)
  # plot(gcc, layout = layout_with_graphopt(g, charge = 1), vertex.color = 1, vertex.frame.color = 1, vertex.size = 3, vertex.label.color = 'black', vertex.label.cex = 0.9, vertex.label.dist = 1)
  # #plot(gcc, layout = layout_with_kk, vertex.color = adjustcolor('darkolivegreen4', 0.1), vertex.frame.color = 'darkolivegreen4', vertex.size = 2*strength(gcc), vertex.label.color = 'black', vertex.label.cex = 0.9, vertex.label.dist = 1, edge.width = 3*E(g)$weight/max(E(g)$weight))
  # title(main = "Giant Connected Component ", outer = T, line = -1)
  # dev.off()
  
  #Skipgrams
  names(text) <- NULL 
  text_df <- tibble(line = 1:length(text), text = text)
  
  ##### tokenizar en skip-gram
  # en este caso cada token es un unigrama o un bigrama regular o un bigrama con espaciamiento
  text_df %>%
    unnest_tokens( input = text, output = skipgram, token = "skip_ngrams", n = 2) %>%
    filter(!is.na(skipgram)) -> text_df_skip
  
  ##### remover unigramas
  suppressMessages(suppressWarnings(library(ngram)))
  # contar palabras en cada skip-gram
  text_df_skip$num_words <- text_df_skip$skipgram %>% 
    map_int(.f = ~ wordcount(.x))
  
  # remover unigramas
  text_df_skip %<>% 
    filter(num_words == 2) %>% 
    select(-num_words)
  
  ##### omitir stop words
  text_df_skip %>%
    separate(skipgram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>%
    filter(!is.na(word1)) %>% 
    filter(!is.na(word2)) %>%
    count(word1, word2, sort = TRUE) %>%
    rename(weight = n) -> text_df_skip_counts
  
  dim(text_df_skip_counts) 
  head(text_df_skip_counts, n = 10)
  
  #definir una red a partir de la frecuencia (weight) de los bigramas
  umbral <- 1
  g <- text_df_skip_counts %>%
    filter(weight >= umbral) %>%
    graph_from_data_frame(directed = FALSE)
  g <- igraph::simplify(g)
  #graph_name <- file.path(PARENT_DIR, "Graphs", paste0("graph_skipgrams_", i, ".gml"))
  graph_name <- file.path(PARENT_DIR, "Graphs", paste0("graph_skipgrams.gml"))
  write.graph(g, graph_name, format = "gml")
  V(g)$cluster <- clusters(graph = g)$membership
  gcc <- induced_subgraph(graph = g, vids = which(V(g)$cluster == which.max(clusters(graph = g)$csize)))
  set.seed(123)
  #png("GCC_skipgram_article.png", width = 15, height = 15, units = "cm", res=400)
  #plot(gcc, layout = layout_with_fr, vertex.color = 1, vertex.frame.color = 1, vertex.size = 3, vertex.label = NA)
  #plot(gcc, layout = layout_with_fr, vertex.color = adjustcolor('darkolivegreen4', 0.1), vertex.frame.color = 'darkolivegreen4', vertex.size = 2*strength(gcc), vertex.label = NA)
  #title(main = "Giant Connected Component (Skipgrams)", outer = T, line = -1)
  #dev.off()
#}