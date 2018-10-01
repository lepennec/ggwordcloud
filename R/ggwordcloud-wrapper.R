#' @export
ggwordcloud <- function (words, freq, scale = c(4, 0.5), min.freq = 3, max.words = Inf,
                         random.order = TRUE, random.color = FALSE, rot.per = 0.1,
                         colors = "black", ordered.colors = FALSE, ...)
{
  last <- 1
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

  ggplot(data = words_df, aes(label = word, size = freq,
                              color = color, angle = angle)) +
    geom_text_wordcloud(tstep = .01, rstep = .01,
                        rm_outside = TRUE, ...) +
    scale_size(range = 5 * c(scale[2],scale[1])) +
    scale_color_identity() +
    theme_minimal()
}
