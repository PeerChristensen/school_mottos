# EDA

library(tidyverse)
library(tidytext)

# load data
df <- read_csv("school_mottos.csv")

nrow(df)

# most universities by country
df %>% group_by(country) %>% count() %>% arrange(desc(n))

# most universities by language
df %>% group_by(Language) %>% count() %>% arrange(desc(n))


#clustering
df = df %>% group_by(country) %>% add_count() %>% filter(n>15) %>% ungroup()

df %<>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)     %>%
  add_count(word)           %>%
  filter(n > 1, word != "")

dfS <- df %>% cast_dfm(country,word,n)

