#' Love in several languages with number of speakers
#'
#' A dataset containing the word love in different languages (147 or 34 for the
#' small one) as well as the number of native speakers and overall speakers of
#' those languages. Latin only version are used in the help.
#'
#' @format a data.frame with 147 observations (or 34 for the small one) of 5
#'   variables
#'   \describe{
#'   \item{iso_639_3}{the ISO 639-3 language code}
#'   \item{word}{the word love in that language}
#'   \item{name}{English name of the language}
#'   \item{native_speakers}{number of native speakers in millions}
#'   \item{speakers}{number of speakers in millions}
#'   }
#'
#' @source wikipedia
"love_words"

#' @rdname love_words
"love_words_small"

#' @rdname love_words
"love_words_latin"

#' @rdname love_words
"love_words_latin_small"
