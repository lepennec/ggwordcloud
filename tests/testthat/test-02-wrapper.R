context("test-wrapper")

dat <- mtcars
dat$name <- row.names(mtcars)
dat$size <- dat$mpg
dat$size[1] <- 300

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
