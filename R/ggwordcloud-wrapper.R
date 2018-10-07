#' wordcloud approximate replacement
#'
#' \code{ggwordcloud} is meant as an approximate replacement for
#' \code{\link[wordcloud]{wordcloud}}. It has almost the same syntax but allows
#' only the words/freqs input. As the underlying algorithms are not strictly
#' equal, the resulting wordcloud is only similar to the ones one can obtain
#' with \code{\link[wordcloud]{wordcloud}}.
#'
#' @param words the words
#' @param freq their frequencies
#' @param scale A vector of length 2 indicating the range of the size of the
#'   words.
#' @param min.freq words with frequency below min.freq will not be plotted
#' @param max.words Maximum number of words to be plotted. least frequent terms
#'   dropped
#' @param random.order plot words in random order. If false, they will be
#'   plotted in decreasing frequency
#' @param random.color choose colors randomly from the colors. If false, the
#'   color is chosen based on the frequency
#' @param rot.per proportion words with 90 degree rotation
#' @param colors color words from least to most frequent
#' @param ordered.colors if true, then colors are assigned to words in order
#' @param ... Additional parameters to be passed to geom_text_wordcloud
#' @return a ggplot
#' @examples
#' set.seed(42)
#' dat <- mtcars
#' dat$name <- row.names(mtcars)
#' dat$size <- dat$mpg
#' dat$size[1] <- 300
#'
#' ggwordcloud(dat$name, dat$size)
#' @export
ggwordcloud <- function (words, freq, scale = c(4, 0.5), min.freq = 3, max.words = Inf,
                         random.order = TRUE, random.color = FALSE, rot.per = 0.1,
                         colors = "black", ordered.colors = FALSE, ...)
{
  nc <- length(colors)
  words_df <- data.frame(word = words, freq = freq)
  if (min.freq > max(freq))
    min.freq <- 0
  ord <- rank(-freq, ties.method = "random")
  words_df <- words_df[ord <= max.words,]
  if (ordered.colors) {
    colors <- colors[ord <= max.words]
  }
  if (random.order)
    ord <- sample.int(nrow(words_df))
  else ord <- order(words_df$freq, decreasing = TRUE)
  words_df <- words_df[ord,]
  words_df <- words_df[words_df$freq >= min.freq,]
  words_df$normedFreq <- words_df$freq/max(words_df$freq)
  if (!random.color) {
    if (ordered.colors) {
      words_df$color <- colors[ord][freq >= min.freq]
    } else {
      words_df$color <- colors[ceiling(nc * words_df$normedFreq)]
    }
  } else {
    words_df$color <- colors[sample.int(nc, nrow(words_df), replace = TRUE)]
  }
  words_df$angle = 90 * (runif(nrow(words_df)) < rot.per)

  with(words_df,
       ggplot(data = words_df, aes(label = word, size = freq,
                                   color = color, angle = angle)) +
         geom_text_wordcloud(rstep = .01, tstep = .02,
                             rm_outside = TRUE, ...) +
         scale_size(range = 5 * c(scale[2],scale[1])) +
         scale_color_identity() +
         theme_minimal()
  )
}

#' wordcloud2 approximate replacement
#'
#' \code{ggwordcloud2} is meant as an approximate replacement for
#' \code{\link[wordcloud2]{wordcloud2}}. It has almost the same syntax but fewer
#' options. In particular, there is no background image (so far...). As the
#' underlying algorithms are not strictly equal, the resulting wordcloud is only
#' similar to the ones one can obtain with \code{\link[wordcloud2]{wordcloud2}}.
#'
#' @param data a dataframe whose two first columns are the names and the freqs
#'   or a table
#' @param size scaling factor. Default to 1
#' @param color color scheme either "random-dark", "random-light" or a list of
#'   color of the size of the dataframe. Default to "random-dark"
#' @param minRotation the minimal rotation angle
#' @param maxRotation the maximal rotation angle
#' @param rotateRation the proportion of rotated words
#' @param shuffle if TRUE, the words are shuffled at the beginning
#' @param ellipticity control the eccentricity of the wordcloud
#' @param ... the remaining parameters are passed to geom_text_wordcloud
#' @return a ggplot
#' @examples
#' set.seed(42)
#' dat <- mtcars
#' dat$name <- row.names(mtcars)
#' dat$size <- dat$mpg
#' dat$size[1] <- 300
#'
#' ggwordcloud2(dat[,c("name", "size")])
#' @export
ggwordcloud2 <- function (data,
                          size = 1,
                          #minSize = 0, gridSize = 0, fontFamily = "Segoe UI",
                          #fontWeight = "bold",
                          color = "random-dark",
                          #backgroundColor = "white",
                          minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE,
                          rotateRatio = 0.4,
                          #shape = "circle",
                          ellipticity = 0.65,
                          ...)
{
  if ("table" %in% class(data)) {
    dataOut = data.frame(name = names(data), freq = as.vector(data))
  }
  else {
    data = as.data.frame(data)
    dataOut = data[, 1:2]
    names(dataOut) = c("name", "freq")
  }

  dataOut$rot <- (minRotation + (maxRotation-minRotation) * runif(nrow(dataOut))) * (runif(dataOut) < rotateRatio) * 45/pi

  if (shuffle) {
    ord <- sample.int(nrow(dataOut))
    dataOut <- dataOut[ord,]
  }

  if (color == "random-dark") {
    dataOut$color <- random_dark(nrow(dataOut))
  } else {
    if (color == "random-light") {
      dataOut$color <- random_dark(nrow(dataOut))
    } else
    {
      dataOut$color <- color
    }
  }

  with(dataOut,
       ggplot(data = dataOut, aes(label = name, size = freq, angle = rot,
                                  color = color)) +
         geom_text_wordcloud(eccentricity = ellipticity, rm_outside = TRUE, ...) +
         scale_color_identity() +
         scale_size(limits  = c(0,NA), range = c(0,18*size)) +
         theme_minimal()
  )
}

random_hsl_color <- function (min, max, n = 1) {
  hex(HLS(360*runif(n),
          (min + (max - min) * runif(n))/100,
          (70 + 30 * runif(n))/100))
}

random_dark <- function(n = 1){
  random_hsl_color(10, 50, n)
}

random_light <- function(n = 1){
  random_hsl_color(50, 90, n)
}
