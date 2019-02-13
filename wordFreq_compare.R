# word frequency compared

library(tidyverse)
library(tidytext)
library(widyr)
library(magrittr)
library(ggraph)
library(igraph)
library(wordcloud)
library(tm)


# Latin vs. not Latin

# load data
df <- read_csv("school_mottos.csv")

# tokenize and remove stop words, group on basis of language
df %<>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)     %>%
  add_count(word,Language == "Latin") %>%
  filter(n > 5, word != "")
  
df$word <- removeNumbers(df$word)

# Create the log odds ratio of each word, group by Latin language
df_ratios_Latin <- df %>%
  count(word, `Language == "Latin"`) %>%
  group_by(word) %>%
  filter(sum(n) >= 5) %>%
  spread(`Language == "Latin"`, n, fill = 0) %>%
  ungroup() %>%
  mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
  mutate(logratio = log2(`FALSE`/ `TRUE`)) %>%
  arrange(desc(logratio))

# Plot 
df_ratios_Latin %>%
  group_by(logratio > 0) %>%
  top_n(20, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Not Latin / Latin log ratio") +
  scale_fill_manual(name = "", labels = c("Not Latin", "Latin"),
                    values = c("red", "lightblue"))
  
# US vs. non-US

# load data
df <- read_csv("school_mottos.csv")

# tokenize and remove stop words, group on basis of language
df %<>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)     %>%
  add_count(word,country == "United_States") %>%
  filter(n > 5, word != "")

df$word <- removeNumbers(df$word)

# Create the log odds ratio of each word
df_ratios_US <- df %>%
  count(word, `country == "United_States"`) %>%
  group_by(word) %>%
  filter(sum(n) >= 5) %>%
  spread(`country == "United_States"`, n, fill = 0) %>%
  ungroup() %>%
  mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
  mutate(logratio = log2(`FALSE`/ `TRUE`)) %>%
  arrange(desc(logratio))

# Plot 
df_ratios_US %>%
  group_by(logratio > 0) %>%
  top_n(20, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Non-US / US log ratio") +
  scale_fill_manual(name = "", labels = c("Non-US", "US"),
                    values = c("red", "lightblue"))

