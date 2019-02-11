# word frequency compared

library(tidyverse)
library(tidytext)
library(widyr)
library(magrittr)
library(ggraph)
library(igraph)
library(wordcloud)

# load data
df <- read_csv("school_mottos.csv")

# tokenize and remove stop words
df %<>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)     %>%
  add_count(word)           %>%
  filter(n > 1, word != "")

# most frequent words
dfFreqUS <- df %>%
  filter(Language == "Latin") %>%
  group_by(word) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  top_n(20,n) %>%
  mutate(row = rev(row_number())) 