context("test-bugfixes")

data("love_words_small")

# Use png device as pdf is not working for strange utf8 characters
tmp_file <- tempfile()
png(filename = tmp_file)

test_that("No issue with geom_text_wordcloud_area when no size is defined", {
  expect_is({
    set.seed(42)
    print(ggplot(data = love_words_small, aes(label = word)) + geom_text_wordcloud_area())
  }, "ggplot")
})

test_that("geom_text_wordcloud does not crash when shape is NA", {
  expect_warning({
    set.seed(42)
    print(ggplot(
      data = love_words_small,
      aes(label = word)
    ) +
      geom_text_wordcloud(shape = NA))
  })
})

test_that("geom_text_wordcloud_area does not crash when shape is NA", {
  expect_warning({
    set.seed(42)
    print(ggplot(
      data = love_words_small,
      aes(label = word, size = speakers)
    ) +
      geom_text_wordcloud_area(shape = NA))
  })
})

lws <- love_words_small
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

file.remove(tmp_file)
