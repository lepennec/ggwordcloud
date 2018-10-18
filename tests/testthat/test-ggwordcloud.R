context("test-ggwordcloud")

dat <- mtcars
dat$name <- row.names(mtcars)
dat$size <- dat$mpg
dat$size[1] <- 300

test_that("geom_text_wordcloud does not crash", {
  expect_is({
    ggplot(data = dat, aes(label = name)) + geom_text_wordcloud()
  }, "ggplot")
})

test_that("geom_text_wordcloud does not crash", {
  expect_is({
    ggplot(data = dat, aes(label = name, size = size)) + geom_text_wordcloud()
  }, "ggplot")
})


context("test-bugfixes")

test_that("No issue with geom_text_wordcloud_area when no size is defined", {
  expect_is({
    ggplot(data = dat, aes(label = name)) + geom_text_wordcloud_area()
  }, "ggplot")
})
