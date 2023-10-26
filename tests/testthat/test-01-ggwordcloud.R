context("test-basic-ggwordcloud")

data("love_words_latin_small")

test_that("geom_text_wordcloud does not crash", {
  expect_is({
    set.seed(42)
    print(ggplot(
      data = love_words_latin_small,
      aes(label = word)
    ) +
      geom_text_wordcloud())
  }, "ggplot")
})

test_that("geom_text_wordcloud_area does not crash", {
  expect_is({
    set.seed(42)
    print(ggplot(
      data = love_words_latin_small,
      aes(label = word, size = speakers)
    ) +
      geom_text_wordcloud_area())
  }, "ggplot")
})

test_that("geom_text_wordcloud works for all shape", {
  expect_equal({
    set.seed(42)
    for (i in 1:8) {
      print(ggplot(
        data = love_words_latin_small,
        aes(label = word)
      ) +
        geom_text_wordcloud(shape = i))
    }
    42
  }, 42)
})

test_that("geom_text_wordcloud works with a mask", {
  expect_is({
    set.seed(42)
    print(ggplot(
      data = love_words_latin_small,
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
      data = love_words_latin_small,
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
      data = love_words_latin_small,
      aes(label = word, size = 1)
    ) +
      geom_text_wordcloud() +
      scale_size_area(max_size = 40))
  })
})

test_that("geom_text_wordcloud complains silently when the words do not fit and the option rm_outside is set to TRUE", {
  expect_warning({
    set.seed(42)
    print(ggplot(
      data = love_words_latin_small,
      aes(label = word, size = 1)
    ) +
      geom_text_wordcloud(rm_outside = TRUE) +
      scale_size_area(max_size = 40))
  })
})

test_that("geom_text_wordcloud complains when one word does not fit", {
  expect_warning({
    set.seed(42)
    print(ggplot(
      data = love_words_latin_small[2, ],
      aes(label = word, size = 1)
    ) +
      geom_text_wordcloud() +
      scale_size_area(max_size = 200))
  })
})

test_that("geom_text_wordcloud complains silently when one word does not fit and the option rm_outside is set to TRUE", {
  expect_warning({
    set.seed(42)
    print(ggplot(
      data = love_words_latin_small[2, ],
      aes(label = word, size = 1)
    ) +
      geom_text_wordcloud(rm_outside = TRUE) +
      scale_size_area(max_size = 200))
  })
})


test_that("geom_text_wordcloud works with a show_boxes = TRUE", {
  expect_is({
    set.seed(42)
    print(ggplot(
      data = love_words_latin_small,
      aes(label = word)
    ) +
      geom_text_wordcloud(show_boxes = TRUE) +
      scale_size_area(max_size = 10))
  }, "ggplot")
})

test_that("geom_text_wordcloud_area does not crash when using label_content", {
  expect_is({
    set.seed(42)
    print(ggplot(
      data = love_words_latin_small,
      aes(label = word, size = speakers,
          label_content = "A")
    ) +
      geom_text_wordcloud_area())
  }, "ggplot")
})


test_that("geom_text_wordcloud_area works with use_richtext = FALSE", {
      expect_is({
        set.seed(42)
        print(ggplot(
          data = love_words_latin_small,
          aes(label = word, size = speakers)
      ) +
        geom_text_wordcloud_area(use_richtext = FALSE))
      }, "ggplot")
    })

test_that("geom_text_wordcloud_area works with parse = TRUE", {
  expect_is({
    set.seed(42)
    love_words_latin_small2 <- love_words_latin_small
    love_words_latin_small2[["word"]] <- "sin(x)"
    print(ggplot(
      data = love_words_latin_small2,
      aes(label = word, size = speakers)
    ) +
      geom_text_wordcloud_area(parse = TRUE))
  }, "ggplot")
})

