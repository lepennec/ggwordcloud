library(tidyverse)
love_words <- read_csv("./data-raw/love_words.csv")
love_words <- amor3 %>% mutate(native_speakers = L1, speakers = L1 + L2) %>% select(-L1, -L2) %>% arrange(desc(speakers))
love_words_small <- love_words %>% filter(speakers > 50)
