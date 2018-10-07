#' ggwordcloud
#'
#' A wordcloud package for ggplot2
#' @keywords internal
#' @rdname ggwordcloud-package
#' @name ggwordcloud-package
#' @docType package
#' @useDynLib ggwordcloud, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @import ggplot2
#' @importFrom scales alpha
#' @import grid
#' @importFrom Cairo Cairo
#' @importFrom grDevices
#'   dev.cur
#'   dev.off
#'   dev.set
#'   dev.size
#' @importFrom colorspace
#'   hex
#'   HLS
#' @importFrom stats runif
NULL
