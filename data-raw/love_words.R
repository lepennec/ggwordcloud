library(tidyverse)
love_words <- read_csv("./data-raw/love_words.csv")
lang_stats <- read_csv("data-raw/lang_stats.csv")
love_words <- love_words %>%
  left_join(lang_stats) %>%
  mutate(native_speakers = L1, speakers = L1 + L2) %>%
  select(-L1, -L2) %>%
  arrange(desc(speakers))
love_words_small <- love_words %>% filter(speakers > 50)
