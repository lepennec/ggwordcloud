context("test-basic-ggwordcloud")

data("love_words_small")

# Use png device as pdf is not working for strange utf8 characters
tmp_file <- tempfile()
png(filename = tmp_file)

test_that("geom_text_wordcloud does not crash", {
  expect_is({
    set.seed(42)
    print(ggplot(
      data = love_words_small,
      aes(label = word)
    ) +
      geom_text_wordcloud())
  }, "ggplot")
})

test_that("geom_text_wordcloud_area does not crash", {
  expect_is({
    set.seed(42)
    print(ggplot(
      data = love_words_small,
      aes(label = word, size = speakers)
    ) +
      geom_text_wordcloud_area())
  }, "ggplot")
})

test_that("geom_text_wordcloud works for all shape", {
  expect_silent({
    set.seed(42)
    for (i in 1:8) {
      print(print(ggplot(
        data = love_words_small,
        aes(label = word)
      ) +
        geom_text_wordcloud(shape = i)))
    }
  })
})

test_that("geom_text_wordcloud works with a mask", {
  expect_is({
    set.seed(42)
    print(ggplot(
      data = love_words_small,
      aes(label = word)
    ) +
      geom_text_wordcloud(mask = png::readPNG(system.file("extdata/hearth.png",
        package = "ggwordcloud", mustWork = TRUE
      ))) +
      scale_size_area(max_size = 10))
  }, "ggplot")
})

test_that("geom_text_wordcloud_area works with a mask", {
  expect_is({
    set.seed(42)
    print(ggplot(
      data = love_words_small,
      aes(label = word)
    ) +
      geom_text_wordcloud_area(mask = png::readPNG(system.file("extdata/hearth.png",
        package = "ggwordcloud", mustWork = TRUE
      ))) +
      scale_size_area(max_size = 10))
  }, "ggplot")
})

test_that("geom_text_wordcloud complains when the words does not fit", {
  expect_warning({
    set.seed(42)
    print(ggplot(
      data = love_words_small,
      aes(label = word, size = 1)
    ) +
      geom_text_wordcloud() +
      scale_size_area(max_size = 20))
  })
})

test_that("geom_text_wordcloud complains silently when the words do not fit and the option rm_outside is set to TRUE", {
  expect_message({
    set.seed(42)
    print(ggplot(
      data = love_words_small,
      aes(label = word, size = 1)
    ) +
      geom_text_wordcloud(rm_outside = TRUE) +
      scale_size_area(max_size = 20))
  })
})

test_that("geom_text_wordcloud complains when one word do not fit", {
  expect_warning({
    set.seed(42)
    print(ggplot(
      data = love_words_small[2, ],
      aes(label = word, size = 1)
    ) +
      geom_text_wordcloud() +
      scale_size_area(max_size = 200))
  })
})

test_that("geom_text_wordcloud complains silently when one word does not fit and the option rm_outside is set to TRUE", {
  expect_message({
    set.seed(42)
    print(ggplot(
      data = love_words_small[2, ],
      aes(label = word, size = 1)
    ) +
      geom_text_wordcloud(rm_outside = TRUE) +
      scale_size_area(max_size = 200))
  })
})
file.remove(tmp_file)
