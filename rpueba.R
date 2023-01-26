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
 text<-c("Acceso carnal violento;El que realice acceso carnal con otra persona mediante violencia, incurrira en prision de doce (12) a veinte (20) años ",
         
         "Acto sexual violento;El que realice en otra persona acto sexual diverso al acceso carnal mediante violencia, incurrira en prision de ocho (8) a dieciseis (16) años ",
         
         "Acceso carnal o acto sexual en persona puesta en incapacidad de resistir; El que realice acceso carnal con persona a la cual haya puesto en incapacidad de resistir o en estado de inconsciencia, o en condiciones de inferioridad siquica que le impidan comprender la relacion sexual o dar su consentimiento, incurrira en prision de doce (12) a veinte (20) años ",
         
         "Acceso carnal abusivo con menor de catorce años;El que acceda carnalmente a persona menor de catorce (14) años, incurrira en prision de doce (12) a veinte (20) años ",
         
         "Actos sexuales con menor de catorce años; El que realizare actos sexuales diversos del acceso carnal con persona menor de catorce (14) años o en su presencia, o la induzca a practicas sexuales, incurrira en prision de nueve (9) a trece (13) años ",
         
         "Acceso carnal o acto sexual abusivos con incapaz de resistir;El que acceda carnalmente a persona en estado de inconsciencia, o que padezca trastorno mental o que este en incapacidad de resistir, incurrira en prision de doce (12) a veinte (20) años ",
         
         "Acoso sexual;El que en beneficio suyo o de un tercero y valiendose de su superioridad manifiesta o relaciones de autoridad o de poder, edad, sexo, posicion laboral, social, familiar o economica, acose, persiga, hostigue o asedie fisica o verbalmente, con fines sexuales no consentidos, a otra persona, incurrira en prision de uno (1) a tres (3) años ",
         
         "Circunstancias de agravacion punitiva; Las penas para los delitos descritos en los articulos anteriores, se aumentaran de una tercera parte a la mitad, cuando:
           La conducta se cometiere con el concurso de otra u otras personas, El responsable tuviere cualquier caracter, posicion o cargo que le de particular autoridad sobre la victima o la impulse a depositar en el su confianza, Se produjere contaminacion de enfermedad de transmision sexual, Se realizare sobre persona menor de catorce (14) años, La conducta se realizare sobre pariente hasta cuarto grado de consanguinidad, cuarto de afinidad o primero civil, sobre conyuge o compañera o compañero permanente, o contra cualquier persona que de manera permanente se hallare integrada a la unidad domestica, o aprovechando la confianza depositada por la victima en el autor o en alguno o algunos de los participes. Para los efectos previstos en este articulo, la afinidad sera derivada de cualquier forma de matrimonio o de union libre, Se produjere embarazo, Si se cometiere sobre personas en situacion de vulnerabilidad en razon de su edad, etnia, discapacidad fisica, psiquica o sensorial, ocupacion u oficio, Si el hecho se cometiere con la intencion de generar control social, temor u obediencia en la comunidad ",
          
         "Circunstancias de agravacion punitiva; Cuando se cometiere uno de los delitos descritos en los articulos 205, 207 o 210 de este Codigo, la pena sera de 480 a 600 meses de prision o pena de prision perpetua revisable, si la victima fuere un menor de dieciocho (18) años y en los siguientes casos: El autor haya aprovechado de una relacion de superioridad, debe ser de cuidado o parentesco con la victima, por ser su pariente hasta el cuarto grado de consanguinidad, segundo de afinidad o primero civil. La conducta se cometiere con sevicia, o mediante actos degradantes o vejatorios. Si el hecho se cometiere con la intencion de generar control social, temor u obediencia en la comunidad. La victima se encontrara en especial situacion de vulnerabilidad en razon de su corta edad, etnia, discapacidad fisica, psiquiatrica o sensorial. La conducta se cometiere con alevosia o ensañamiento, aumentando deliberadamente inhumanamente el dolor de la victima.  La conducta se consuma en un contexto de violencia de genero.  Se someta a la victima a tratos crueles, inhumanos o degradantes. El autor ha perpetuado multiples conductas punibles de las contenidas en los articulos 205, 207 y 211 del Codigo Penal ",
     
          "Acceso carnal; Para los efectos de las conductas descritas en los capitulos anteriores, se entendera por acceso carnal la penetracion del miembro viril por via anal, vaginal u oral, asi como la penetracion vaginal o anal de cualquier otra parte del cuerpo humano u otro objeto ",

          " Violencia; Para los efectos de las conductas descritas en los capitulos anteriores, se entendera por violencia: el uso de la fuerza; la amenaza del uso de la fuerza; la coaccion fisica o psicologica, como la causada por el temor a la violencia, la intimidacion; la detencion ilegal; la opresion psicologica; el abuso de poder; la utilizacion de entornos de coaccion y circunstancias similares que impidan a la victima dar su libre consentimiento ")

 library(dplyr)
 text_df <- tibble(line = 1:11, text = text)
 
 
 # 4 Tokenización
 
 text_df %<>%
   unnest_tokens(input = text, output = word) %>%
   filter(!is.na(word))  # importante!
 
 class(text_df)
 # 5 Normalización del texto
 
 ##### texto con numeros?
 text_df %>%
   filter(grepl(pattern = '[0-9]', x = word)) %>% 
   count(word, sort = TRUE)
   
   
   ##### remover texto con numeros
   # ---------- petro ----------
 text_df %<>%
   filter(!grepl(pattern = '[0-9]', x = word))
 dim(text_df)
 
 
 ##### stop words 
 # 3 diccionarios en ingles (onix, SMART, snowball) incluidos por defecto en tidytext
 data(stop_words)
 class(stop_words)
 
 dim(stop_words)
 ###### stop words 
 # no hay diccionarios en español disponibles en tidytext
 # diccionario COUNTWORDSFREE en español (con acentos)
 # http://countwordsfree.com/stopwords/spanish
 # otras alternativas:
 #   https://github.com/stopwords-iso/stopwords-es
 #   de tm::stopwords("spanish")
 # se conserva el mismo formato de los diccionarios en tidytext
 stop_words_es <- tibble(word = unlist(c(read.table("~/Research Articles/Concept Theory + NLP/stop_words_spanish.txt", quote="\"", comment.char=""))), lexicon = "custom")
 dim(stop_words_es)
 
 head(stop_words_es, n = 10)

 
 text_df %<>% 
   anti_join(x = ., y = stop_words_es) 
 
 
 ##### remover acentos
 replacement_list <- list('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u')
 # ---------- petro ----------
 text_df %<>% 
   mutate(word = chartr(old = names(replacement_list) %>% str_c(collapse = ''), 
                        new = replacement_list %>% str_c(collapse = ''),
                        x = word))
 dim(text_df)
 
 
 # 6 Tokens más frecuentes
 
 ##### top 10 de tokens mas frecuentes
 # ---------- petro ----------
 text_df %>% 
   count(word, sort = TRUE) %>%
   head(n = 10)
 
 
 ##### viz
 suppressMessages(suppressWarnings(library(gridExtra)))
 # ---------- petro ----------
 text_df %>%
   count(word, sort = TRUE) %>%
   filter(n > 7) %>%
   mutate(word = reorder(word, n)) %>%
   ggplot(aes(x = word, y = n)) +
   theme_light() + 
   geom_col(fill = 'darkolivegreen4', alpha = 0.8) +
   xlab(NULL) +
   ylab("Frecuencia") +
   coord_flip() +
   ggtitle(label = ' Conteo de palabras') -> p1
 
 grid.arrange(p1)

 
 suppressMessages(suppressWarnings(library(wordcloud)))
 ###### viz
 par(mfrow = c(1,2), mar = c(1,1,2,1), mgp = c(1,1,1))
 # ---------- petro ----------
 set.seed(123)
 text_df %>%
   count(word, sort = TRUE) %>%
   with(wordcloud(words = word, freq = n, max.words = 20, colors = 'darkolivegreen4'))
 title(main = "violencia") 
 
 
 ##### frecuencias relativas de la palabras
 bind_rows(mutate(.data = text_df, author = "Articulos")) %>%
   count(author, word) %>%
   group_by(author) %>%
   mutate(proportion = n/sum(n)) %>%
   select(-n) %>%
   spread(author, proportion, fill = 0) -> frec  # importante!
 frec %<>% 
   select(word, Articulos)
 dim(frec)
 
 head(frec, n = 10)

 
 ##### top 10 palabras en comun
 # orden anidado respecto a petro y duque
 frec %>%
   filter(Articulos !=0) %>%
   arrange(desc(Articulos)) -> frec_comun
 dim(frec_comun) 
 
 head(frec_comun, n = 10)

 
 ###### proporcion palabras en comun
 dim(frec_comun)[1]/dim(frec)[1] 
 
 
 
 #   7 Análisis de sentimiento
 
 
 ##### sentiments 
 # 3 diccionarios en ingles (AFINN, Bing, NRC) incluidos por defecto en tidytext
 # AFINN: Finn Arup Nielsen, escala de -5 a 5.
 #   http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010
 # Bing: Bing Liu and collaborators, clasificacion binaria (+/-).
 #   https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
 # NRC: Saif Mohammad and Peter Turney, clasificacion binaria (+/-) y algunas categorias.
 #   http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
 
 positive_words <- read.csv("~/Research Articles/Concept Theory + NLP/positive_words_es.txt", sep="") %>%
   mutate(sentiment = "Positivo")
 colnames(positive_words)[1] <- "word"
 negative_words <- read.csv("~/Research Articles/Concept Theory + NLP/negative_words_es.txt", sep="") %>%
   mutate(sentiment = "Negativo")
 colnames(negative_words)[1] <- "word"
 sentiment_words <- bind_rows(positive_words, negative_words)
 # comparacion de diccionarios
 get_sentiments("bing") %>%
   count(sentiment)
 
 sentiment_words %>%
   count(sentiment)
 
 
 ###### viz
 suppressMessages(suppressWarnings(library(RColorBrewer)))
 # ---------- Articulo ----------
 text_df %>%
   inner_join(sentiment_words) %>%
   count(word, sentiment, sort = TRUE) %>%
   filter(n > 2) %>%
   mutate(n = ifelse(sentiment == "Negativo", -n, n)) %>%
   mutate(word = reorder(word, n)) %>%
   ggplot(aes(word, n, fill = sentiment)) +
   geom_col() +
   scale_fill_manual(values = brewer.pal(8,'Dark2')[c(2,5)]) +
   coord_flip(ylim = c(-7,7)) +
   labs(y = "Frecuencia",
        x = NULL,
        title = "Aticulos: Conteo por sentiment") +
   theme_minimal() -> p1
 
 
 # desplegar grafico
 grid.arrange(p1)
 
 
 suppressMessages(suppressWarnings(library(reshape2)))  # acast
 ##### viz
 par(mfrow = c(1,2), mar = c(1,1,2,1), mgp = c(1,1,1))
 # ---------- petro ----------
 set.seed(123)
 text_df %>%
   inner_join(sentiment_words) %>%
   count(word, sentiment, sort = TRUE) %>%
   acast(word ~ sentiment, value.var = "n", fill = 0) %>%
   comparison.cloud(colors = brewer.pal(8,'Dark2')[c(2,5)], 
                    max.words = 50, title.size = 1.5)
 
 
 
 ##8  Bigramas
 names(text) <- NULL 
 text_df <- tibble(line = 1:length(text), text = text)

 
 ##### tokenizar en bigramas
 # en este caso cada token es un bigrama
 text_df %>%
   unnest_tokens(input = text, output = bigram, token = "ngrams", n = 2) %>%
   filter(!is.na(bigram)) -> text_df_bi # importante!
 dim(text_df_bi)
 
 
 head(text_df_bi, n = 10)

 ###### top 10 de bigramas mas frecuentes
 # hay bigramas que no son interesantes (e.g., "de la")
 # esto motiva el uso de stop words nuevamente
 text_df_bi %>%
   count(bigram, sort = TRUE) %>%
   head(n = 10)
 
 
 ##### omitir stop words
 text_df_bi %>%
   separate(bigram, c("word1", "word2"), sep = " ") %>%
   filter(!grepl(pattern = '[0-9]', x = word1)) %>%
   filter(!grepl(pattern = '[0-9]', x = word2)) %>%
   filter(!word1 %in% stop_words_es$word) %>%
   filter(!word2 %in% stop_words_es$word) %>%
   mutate(word1 = chartr(old = names(replacement_list) %>% str_c(collapse = ''), 
                         new = replacement_list %>% str_c(collapse = ''),
                         x = word1)) %>%
   mutate(word2 = chartr(old = names(replacement_list) %>% str_c(collapse = ''), 
                         new = replacement_list %>% str_c(collapse = ''),
                         x = word2)) %>%
   filter(!is.na(word1)) %>% 
   filter(!is.na(word2)) %>%
   count(word1, word2, sort = TRUE) %>%
   rename(weight = n) -> text_df_bi_counts  # importante para la conformacion de la red!
 dim(text_df_bi_counts)
 
 
 
 head(text_df_bi_counts, n = 10)

 ##### definir una red a partir de la frecuencia (weight) de los bigramas
 # binaria, no dirigida, ponderada, simple
 # se recomienda variar el umbral del filtro y construir bigramas no consecutivos para obtener redes con mayor informacion
 suppressMessages(suppressWarnings(library(igraph)))
 g <- text_df_bi_counts %>%
   filter(weight > 2) %>%
   graph_from_data_frame(directed = FALSE)
 # viz
 set.seed(123)
 plot(g, layout = layout_with_fr, vertex.color = 1, vertex.frame.color = 1, vertex.size = 3, vertex.label.color = 'black', vertex.label.cex = 1, vertex.label.dist = 1, main = "Umbral = 3") 
 
 
 
 ##### red con un umbral diferente
 g <- text_df_bi_counts %>%
   filter(weight > 0) %>%
   graph_from_data_frame(directed = FALSE)
 # viz
 set.seed(123)
 plot(g, layout = layout_with_kk, vertex.color = 1, vertex.frame.color = 1, vertex.size = 3, vertex.label = NA, main = "Umbral = 1")
 
 
 ##### componente conexa mas grande de la red
 g <- text_df_bi_counts %>%
   filter(weight > 0) %>%
   graph_from_data_frame(directed = FALSE)
 # grafo inducido por la componente conexa
 V(g)$cluster <- clusters(graph = g)$membership
 gcc <- induced_subgraph(graph = g, vids = which(V(g)$cluster == which.max(clusters(graph = g)$csize)))
 par(mfrow = c(1,2), mar = c(1,1,2,1), mgp = c(1,1,1))
 # viz 1
 set.seed(123)
 plot(gcc, layout = layout_with_kk, vertex.color = 1, vertex.frame.color = 1, vertex.size = 3, vertex.label.color = 'black', vertex.label.cex = 0.9, vertex.label.dist = 1)
 # viz 2
 set.seed(123)
 plot(gcc, layout = layout_with_kk, vertex.color = adjustcolor('darkolivegreen4', 0.1), vertex.frame.color = 'darkolivegreen4', vertex.size = 2*strength(gcc), vertex.label.color = 'black', vertex.label.cex = 0.9, vertex.label.dist = 1, edge.width = 3*E(g)$weight/max(E(g)$weight))
 title(main = "Componente conexa", outer = T, line = -1)
 
 
 
 
 ##### importar datos

 
 names(text) <- NULL 
 text_df <- tibble(line = 1:length(text), text = text)
 
 ##### tokenizar en skip-gram
 # en este caso cada token es un unigrama o un bigrama regular o un bigrama con espaciamiento
 text_df %>%
   unnest_tokens( input = text, output = skipgram, token = "skip_ngrams", n = 2) %>%
   filter(!is.na(skipgram)) -> text_df_skip
 dim(text_df_skip)
 
 head(text_df_skip, n = 10)
 
 
 ##### remover unigramas
 suppressMessages(suppressWarnings(library(ngram)))
 # contar palabras en cada skip-gram
 text_df_skip$num_words <- text_df_skip$skipgram %>% 
   map_int(.f = ~ wordcount(.x))
 head(text_df_skip, n = 10)
 
 
 # remover unigramas
 text_df_skip %<>% 
   filter(num_words == 2) %>% 
   select(-num_words)
 dim(text_df_skip)
 
 head(text_df_skip, n = 10)

 
 ##### omitir stop words
 text_df_skip %>%
   separate(skipgram, c("word1", "word2"), sep = " ") %>%
   filter(!grepl(pattern = '[0-9]', x = word1)) %>%
   filter(!grepl(pattern = '[0-9]', x = word2)) %>%
   filter(!word1 %in% stop_words_es$word) %>%
   filter(!word2 %in% stop_words_es$word) %>%
   mutate(word1 = chartr(old = names(replacement_list) %>% str_c(collapse = ''), 
                         new = replacement_list %>% str_c(collapse = ''),
                         x = word1)) %>%
   mutate(word2 = chartr(old = names(replacement_list) %>% str_c(collapse = ''), 
                         new = replacement_list %>% str_c(collapse = ''),
                         x = word2)) %>%
   filter(!is.na(word1)) %>% 
   filter(!is.na(word2)) %>%
   count(word1, word2, sort = TRUE) %>%
   rename(weight = n) -> text_df_skip_counts
 dim(text_df_skip_counts) 
 
 head(text_df_skip_counts, n = 10)

 
 ##### definir una red a partir de la frecuencia (weight) de los bigramas
 g <- text_df_skip_counts %>%
   filter(weight > 0) %>%
   graph_from_data_frame(directed = FALSE)
 g <- igraph::simplify(g)  # importante!
 # grafo inducido por la componente conexa
 V(g)$cluster <- clusters(graph = g)$membership
 gcc <- induced_subgraph(graph = g, vids = which(V(g)$cluster == which.max(clusters(graph = g)$csize)))
 par(mfrow = c(1,2), mar = c(1,1,2,1), mgp = c(1,1,1))
 # viz 1
 set.seed(123)
 plot(gcc, layout = layout_with_fr, vertex.color = 1, vertex.frame.color = 1, vertex.size = 3, vertex.label = NA)
 # viz 2
 set.seed(123)
 plot(gcc, layout = layout_with_fr, vertex.color = adjustcolor('darkolivegreen4', 0.1), vertex.frame.color = 'darkolivegreen4', vertex.size = 2*strength(gcc), vertex.label = NA)
 title(main = "Componente conexa", outer = T, line = -1) 