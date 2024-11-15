#устанавливаем библиотеки
install.packages("jsonlite")
install.packages("tibble")

#работа с корпусом
install.packages("quanteda")
install.packages("quanteda.textplots")
install.packages("quanteda.textstats")
install.packages("readtext")
install.packages("tm")

#очистка данных
install.packages("tidyverse")
install.packages("tidytext")
install.packages("stringr")
install.packages("dplyr")

#визуализация
install.packages("RColorBrewer")
install.packages("ggplot2")

#создание сетей
install.packages("factoextra") #кластеризация
install.packages("ggnetwork")
install.packages("linkcomm")
install.packages("tnet")
install.packages("network")



#загружаем библиотеки
#чтение файлов, работа с таблицами
library(readr)
library(jsonlite)
library(tibble)

#работа с корпусом
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(readtext)
library(tm)

#очистка данных
library(tidyverse)
library(tidytext)
library(stringr)
library(dplyr)

#визуализация
library(RColorBrewer)
library(ggplot2)

#создание сетей
library(factoextra) #кластеризация
library(ggnetwork)
library(linkcomm)
library(tnet)
library(network)

#словарь эмоционального окраса слов
dict <- read_csv("russian.csv")

#словарь английских букв, дабы убрать некоторые названия, термины и т.д.
english_letter <- read_csv("english-letter.csv")

#загрузка словаря стоп-слов
#от отвратительный, но это лучше, чем ничего
stopwords_ru <- read_csv("stopwords-ru.txt")

#создаем словарь англ алфавита
english_letter$letterlow <- english_letter$letter
english_letter$letterlow <- tolower(english_letter$letterlow)

#mystem - лучшая программа для лемматизации и стемминга
mystem <- function(doc) {
  library(stringr)
  Myfield = 'mystem.exe -nl -e utf-8'
  sdoc <- system(Myfield, intern=T, input=doc)
  sdoc <- str_replace(sdoc, '\\|.*$', '')
  sdoc <- str_replace(sdoc, '\\?', '')
  sdoc <- paste(sdoc, collapse=" ")
  attributes(sdoc) <- attributes(doc)
  sdoc
}

#прочитаем сообщения
#но мы не будем работать с этим корпусом
df <- fromJSON(file("channel_messages.json"))
df <- data.frame(date = df$date, message = df$message)
df$date <- as.Date(df$date, "%Y-%m-%d")

#очищаем данные
df$ID <- seq.int(nrow(df))
colnames(df) <- c("date", "text", "doc_id")
df <- df %>% filter(text != "")
df <- df %>% filter(text != "   ")
df <- df %>% filter(text != "  ")
df <- df %>% filter(text != "NA")

for (i in 1:nrow(english_letter)) {
  df$text <- str_replace_all(df$text, english_letter$letter[i], "")
  df$text <- str_replace_all(df$text, english_letter$letterlow[i], "")
}


#мы начинаем отсюда
df <- read.csv("df.csv")
df$X <- NULL

#создаем корпус
my.corpus <- corpus(df)
my.corpus.stats <- summary(my.corpus)
my.corpus.stats
write.csv(my.corpus.stats, "corpstat.csv")
my.corpus.stats$Text <- as.numeric(my.corpus.stats$Text)

#оценка корпуса
ggplot(my.corpus.stats, aes(Sentences, Types, group = 1)) + 
  geom_line() + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  ggtitle("Уникальные слова") + xlab("") + ylab("")

ggplot(my.corpus.stats, aes(Text, Sentences, group = 1)) + 
  geom_line() + geom_point() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  ggtitle("Кол-во предложений на сообщение") + xlab("") + ylab("")

ggplot(my.corpus.stats, aes(Tokens, Types, group = 1, label = Text)) + 
  geom_smooth(method = "lm", se = FALSE) + geom_text(check_overlap = T) + 
  ggtitle("Соотношение общего числа слов и уникальных слов")



#отчистка данных через бмблмотеку tm

#убираем сообщения с малым количество текста
df <- df[nchar(df$text) > 100,]
df$text <- sapply(df$text, mystem)
df$text <- str_replace_all(df$text, "\\W?", " ")
df$text <- str_replace_all(df$text, "(?<=\\w)[[:space:]]{1}", "")
df$text <- str_squish(df$text)
df$text <- str_replace_all(df$text, "\\/", "")
df$text <- lapply(df$text, removeWords, c("тот", "это", "наш", "который", 
                                          "свой", "такой", "другой", "чтото", 
                                          "мой", "какой", "очень", "сам", 
                                          "первый", "один", "изза", "однако", 
                                          "весь", "этот","автор", "a", "который", 
                                          "ваш", "наш",
                                          "мочь", "лишь", "хотя", "т", "так", "в",
                                          "да", "как", "из", "за", "на", "нет", "ни",
                                          "но", "о", "об", "из-за", "не", "ещё", 
                                          "я", "с", "и", "что", "кк", "он",
                                          "они", "то", "у", "от", 
                                          "мы", "она", "по", "для"))
df$text <- lapply(df$text, removeWords, stopwords_ru)
df$text <- as.character(df$text)
df$text <- lapply(df$text, removeNumbers)
df$text <- lapply(df$text, tolower)
df$text <- lapply(df$text, stripWhitespace)
df$text <- lapply(df$text, removePunctuation)
df$text <- as.character(df$text)

#создаем именованный id
df$date_id <- paste(df$date, df$doc_id, sep = "__")

#способ обработки через корпус
my.corpus <- corpus(df, docid_field = "date_id",
                    text_field = "text")
my.dfm <- dfm(my.corpus, remove_numbers = TRUE, 
              remove_punct = TRUE, remove_symbols = TRUE, 
              remove = stopwords("russian"))

#облако слов
textplot_wordcloud(my.dfm, min_size = 1, max_size = 5, max_words = 100)
textplot_wordcloud(my.dfm, 
                   color = brewer.pal(4, "Set1"), 
                   min_size = 1, max_size = 8)

textplot_wordcloud(my.dfm[1:8,], color = brewer.pal(4, "Set1"), min_size = 2, max_size = 8, max_words = 1000, 
                   comparison = TRUE)


#сюжеты
concordance <- kwic(my.corpus, "украина", window = 10)
concordance
write.csv(concordance, "concordance.csv")


#создание сети
df$text <- str_squish(df$text)
colnames(df) <- c("date", "kwords", "id")
df$kwords <- trimws(df$kwords)

df <- df[1:100,]
df$kwords <- str_replace_all(df$kwords, "[:space:]", ",")
df$kwords <- str_replace_all(df$kwords, ",,", ",")

e <- df$kwords %>%
  str_split(",") %>%
  lapply(function(x) {
    expand.grid(x, x, w = 1 / length(x), stringsAsFactors = FALSE)
  }) %>%
  bind_rows

e <- apply(e[, -3], 1, str_sort) %>%
  t %>%
  data.frame(stringsAsFactors = FALSE) %>%
  mutate(w = e$w)
e <- group_by(e, X1, X2) %>%
  summarise(w = sum(w)) %>%
  filter(X1 != X2)

e <- e %>% filter(w >= 0.5)
e <- as.data.frame(e)

n <- network(e[, -3], directed = FALSE)

stopifnot(nrow(e) == network.edgecount(n))
set.edge.attribute(n, "weight", e$w)

# weighted degree at alpha = 1
t <- as.edgelist(n, attrname = "weight") %>%
  symmetrise_w %>%
  as.tnet %>%
  degree_w

stopifnot(nrow(t) == network.size(n))
set.vertex.attribute(n, "degree_w", t[, "output" ])


l <- n %v% "degree_w"
l <- network.vertex.names(n)

stopifnot(length(l) == network.size(n))
set.vertex.attribute(n, "label", l)


ggplot(n, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(aes(color = weight)) +
  geom_nodes(color = "black", size = 4) +
  geom_nodes(color = "white", size = 3) +
  geom_text(aes(label = label, color = weight, size = weight)
            , check_overlap = TRUE,   nudge_y = 0.05, size = 5) +
  scale_size_continuous(range = c(0.5, 5)) +
  scale_color_gradient2(low = "darkred", midpoint = 0.35, high = "darkblue") +
  guides(size = FALSE, color = FALSE) + 
  theme_blank()


#сетевая кластеризация
N <- as.data.frame.network(n)
G2 <- graph_from_data_frame(N, directed = FALSE)

krack_full_comm_fg <- fastgreedy.community(G2, modularity = TRUE, 
                                           membership = TRUE, weights = N$weight)
par(bg="white")
plot(krack_full_comm_fg, 
     G2, 
     edge.size = 1,
     vertex.size = 1,
     vertex.label.color="black",
     vertex.label.cex=1,
     label.dist = 1,
     palette = brewer.pal(9, "Greys"))




#интерактивная сеть
g <- graph.data.frame(N)
V(g)$community <- membership(krack_full_comm_fg)
colnames(e) <- c("from", "to", "weight")
edges <- e
nodes <- c(e$from, e$to)
nodes <- unique(nodes)
nodes <- data.frame(nodes)
nodes <- nodes %>% rowid_to_column("id")
edges <- edges %>% left_join(nodes, by = c("from" = "nodes"))
edges <- edges %>% left_join(nodes, by = c("to" = "nodes"))
edges <- edges[,3:5]
edges <- data.frame(edges$id.x, edges$id.y, edges$weight)
colnames(edges) <- c("from", "to", "weight")
nodes_d3 <- mutate(nodes, id = id - 1)
edges_d3 <- mutate(edges, from = from - 1, to = to - 1)
sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", NodeID = "nodes", Value = "weight", fontSize = 7, nodeWidth = 4)

