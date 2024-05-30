#' ggwordcloud
#'
#' A wordcloud package for ggplot2
#' @keywords internal
#' @rdname ggwordcloud-package
#' @name ggwordcloud-package
#' @docType _PACKAGE
#' @useDynLib ggwordcloud, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @import ggplot2
#' @importFrom scales alpha
#' @import grid
#' @importFrom grDevices
#'   dev.cur
#'   dev.off
#'   dev.set
#'   dev.size
#'   png
#' @importFrom colorspace
#'   hex
#'   HLS
#' @importFrom stats runif
#' @importFrom png readPNG
#' @importFrom scales trans_new
#' @importFrom gridtext richtext_grob
NULL
