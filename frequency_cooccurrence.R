# word frequency and cooccurrences

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

dfFreqUS %>%
  ggplot(aes(x=row,y=n)) +
    geom_col(show.legend = FALSE,width = .9) +
    scale_x_continuous( 
    breaks = dfFreqUS$row,
    labels = dfFreqUS$word,
    expand = c(0,0)) +
  coord_flip() 
    
# co-occurence
word_pairs <- df %>%
  pairwise_count(word, University, sort = TRUE)

set.seed(61231)

pairs_plot <- word_pairs %>%
  filter(n > 2)                  %>%
  graph_from_data_frame()        %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n, edge_colour = log(n)),show.legend=F) +
  geom_node_point(size = 4) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void() 

pairs_plot

# wordcloud
wordcloud(df$word)


