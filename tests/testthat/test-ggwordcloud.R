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

context("test-wrapper")

test_that("ggwordcloud does not crash", {
  expect_is({
    set.seed(42)
    print(ggwordcloud(dat$name, dat$size))
  }, "ggplot")
})

test_that("ggwordcloud works with random.color = TRUE", {
  expect_is({
    set.seed(42)
    print(ggwordcloud(dat$name, dat$size, random.color = TRUE))
  }, "ggplot")
})

test_that("ggwordcloud works with random.order = FALSE", {
  expect_is({
    set.seed(42)
    print(ggwordcloud(dat$name, dat$size, random.order = FALSE))
  }, "ggplot")
})

test_that("ggwordcloud2 does not crash", {
  expect_is({
    set.seed(42)
    print(ggwordcloud2(dat[,c("name","size")]))
  }, "ggplot")
})

test_that("ggwordcloud2 works with random-light color", {
  expect_is({
    set.seed(42)
    print(ggwordcloud2(dat[,c("name","size")],
                       color = "random-light"))
  }, "ggplot")
})

test_that("ggwordcloud2 works with explicit colors", {
  expect_is({
    set.seed(42)
    print(ggwordcloud2(dat[,c("name","size")],
                       color = sample(c("black", "red", "green"),
                                      nrow(dat), replace = TRUE)))
  }, "ggplot")
})


context("test-bugfixes")

test_that("No issue with geom_text_wordcloud_area when no size is defined", {
  expect_is({
    set.seed(42)
    print(ggplot(data = dat, aes(label = name)) + geom_text_wordcloud_area())
  }, "ggplot")
})
