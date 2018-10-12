context("test-ggwordcloud")

test_that("ggwordcloud works", {
  expect_is({
    dat <- mtcars
    dat$name <- row.names(mtcars)
    ggplot(data = dat, aes(label = name)) + geom_text_wordcloud()
  }, "ggplot")
})
