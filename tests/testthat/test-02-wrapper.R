context("test-wrapper")

data("love_words_small")

# Use png device as pdf is not working for strange utf8 characters
tmp_file <- tempfile()
png(filename = tmp_file)

test_that("ggwordcloud does not crash", {
  expect_is({
    set.seed(42)
    print(ggwordcloud(love_words_small$word, love_words_small$speakers))
  }, "ggplot")
})

test_that("ggwordcloud works with random.color = TRUE", {
  expect_is({
    set.seed(42)
    print(ggwordcloud(love_words_small$word, love_words_small$speakers, random.color = TRUE))
  }, "ggplot")
})

test_that("ggwordcloud works with random.order = FALSE", {
  expect_is({
    set.seed(42)
    print(ggwordcloud(love_words_small$word, love_words_small$speakers, random.order = FALSE))
  }, "ggplot")
})

test_that("ggwordcloud2 does not crash", {
  expect_is({
    set.seed(42)
    print(ggwordcloud2(love_words_small[, c("word", "speakers")]))
  }, "ggplot")
})

test_that("ggwordcloud2 works with random-light color", {
  expect_is({
    set.seed(42)
    print(ggwordcloud2(love_words_small[, c("word", "speakers")],
      color = "random-light"
    ))
  }, "ggplot")
})

test_that("ggwordcloud2 works with explicit colors", {
  expect_is({
    set.seed(42)
    print(ggwordcloud2(love_words_small[, c("word", "speakers")],
      color = sample(c("black", "red", "green"),
        nrow(love_words_small),
        replace = TRUE
      )
    ))
  }, "ggplot")
})

file.remove(tmp_file)
