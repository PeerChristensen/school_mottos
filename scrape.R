# scrape and clean data, write csv

library(tidyverse)
library(rvest)

url <- read_html("https://en.wikipedia.org/wiki/List_of_university_mottos")

# list countries
countries <- url %>%
  html_nodes("h2 span") %>% 
   html_attr("id") %>%
   na.omit() %>%
   as.vector()

countries <- countries[!countries %in% "References"]

# rows per country
n_rows <- url %>% 
  html_nodes("[class='wikitable']") %>%
  html_table(fill=T) %>% 
  map(nrow) %>%
  unlist()

# scrape and combine tables
df <- url %>% 
  html_nodes("[class='wikitable']") %>%
  html_table(fill=T) %>% bind_rows()

df <- df[,1:4]

# add text column (keep English text, and texts translated into English)
df$text <- ifelse(df$Language == "English",df$Motto,df$Translation)

# add country column
df$country <- rep(countries,n_rows)

write_csv("school_mottos.csv")
