library(tidyverse)
love_words <- read_csv("./data-raw/love_words.csv")
lang_stats <- read_csv("data-raw/lang_stats.csv")
love_words <- love_words |>
  filter(!is.na(word)) |>
  left_join(lang_stats) |>
  mutate(native_speakers = L1, speakers = L1 + L2) %>%
  select(-L1, -L2) |>
  arrange(desc(speakers))
love_words_small <- love_words |>  filter(speakers > 50)
love_words_latin <- love_words |>
  filter(!is.na(iconv(word, from="utf-8", to="latin1") == word)) |>
  mutate(word = iconv(word, from="utf-8", to="latin1"))
love_words_latin_small <- love_words_latin |> filter(speakers > 50)
save(love_words, file = "./data/love_words.rda")
save(love_words_small, file = "./data/love_words_small.rda")
save(love_words_latin, file = "./data/love_words_latin.rda")
save(love_words_latin_small, file = "./data/love_words_latin_small.rda")
