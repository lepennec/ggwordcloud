context("test-basic-ggwordcloud")

dat <- mtcars
dat$name <- row.names(mtcars)
dat$size <- dat$mpg
dat$size[1] <- 300

test_that("geom_text_wordcloud does not crash", {
  expect_is({
    set.seed(42)
    print(ggplot(data = dat, aes(label = name)) + geom_text_wordcloud())
  }, "ggplot")
})

test_that("geom_text_wordcloud_area does not crash", {
  expect_is({
    set.seed(42)
    print(ggplot(data = dat, aes(label = name, size = size)) + geom_text_wordcloud_area())
  }, "ggplot")
})

test_that("geom_text_wordcloud works for all shape", {
  expect_silent({
    set.seed(42)
    for (i in 1:8) {
      print(ggplot(data = dat, aes(label = name)) +
              geom_text_wordcloud(shape = i))
    }
  })
})
