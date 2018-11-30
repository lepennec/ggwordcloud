library(tidyverse)
thankyou_words <- read_csv("./data-raw/thankyou_words.csv")
lang_stats <- read_csv("data-raw/lang_stats.csv")
thankyou_words <- thankyou_words %>%
  filter(!is.na(word)) %>%
  left_join(lang_stats) %>%
  mutate(native_speakers = L1, speakers = L1 + L2) %>%
  select(-L1, -L2) %>%
  arrange(desc(speakers))
thankyou_words_small <- thankyou_words %>% filter(speakers > 50)
