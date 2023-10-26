context("test-bugfixes")

data("love_words_latin_small")

test_that("No issue with geom_text_wordcloud_area when no size is defined", {
  expect_is({
    set.seed(42)
    print(ggplot(data = love_words_latin_small, aes(label = word)) + geom_text_wordcloud_area())
  }, "ggplot")
})

test_that("geom_text_wordcloud does not crash when shape is NA", {
  expect_warning({
    set.seed(42)
    print(ggplot(
      data = love_words_latin_small,
      aes(label = word)
    ) +
      geom_text_wordcloud(shape = NA))
  })
})

test_that("geom_text_wordcloud_area does not crash when shape is NA", {
  expect_warning({
    set.seed(42)
    print(ggplot(
      data = love_words_latin_small,
      aes(label = word, size = speakers)
    ) +
      geom_text_wordcloud_area(shape = NA))
  })
})

lws <- love_words_latin_small
lws$word <- " "
test_that("geom_text_wordcloud does not crash with empty words", {
  expect_is({
    set.seed(42)
    print(ggplot(
      data = lws,
      aes(label = word)
    ) +
      geom_text_wordcloud())
  }, "ggplot")
})


test_that("geom_text_wordcloud works with parse = TRUE", {
  expect_silent({
    set.seed(42)
    print(ggplot(
      data = love_words_latin_small[2,],
      aes(label = word)
    ) +
      geom_text_wordcloud(parse = TRUE))
  })
})

test_that("geom_text_wordcloud works when grid_size = max_grid_size", {
  expect_is({
    set.seed(42)
    print(ggplot(
      data = love_words_latin_small,
      aes(label = word, size = speakers)
    ) +
      geom_text_wordcloud(grid_size = 4, max_grid_size = 4))
  }, "ggplot")
})

