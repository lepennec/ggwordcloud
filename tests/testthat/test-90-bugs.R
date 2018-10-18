context("test-bugfixes")

dat <- mtcars
dat$name <- row.names(mtcars)
dat$size <- dat$mpg
dat$size[1] <- 300

test_that("No issue with geom_text_wordcloud_area when no size is defined", {
  expect_is({
    set.seed(42)
    print(ggplot(data = dat, aes(label = name)) + geom_text_wordcloud_area())
  }, "ggplot")
})
