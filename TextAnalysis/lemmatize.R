#очистка данных
library(tidytext)
library(stringr)
library(dplyr)
library(textstem)

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

#для английского
sample_text <- c("running", "ran", "runner", "better", "best", "studies", "studying",
                 "played", "playing", "happier", "happiest", "feet", "mice")

lemmatized_text <- lemmatize_words(sample_text)
print(lemmatized_text)