
#' A signed power transform
#'
#' @param power power exponent of the direct transform
#'
#' @export
power_trans <- function(power = 1)
{
  force(power)
  trans <- function(x) sign(x)*(abs(x)**power)
  inv <- function(x) sign(x)*(abs(x)**(1/power))
  trans_new(paste0("power-", format(power)), trans, inv,
            domain = c(-Inf, Inf))
}
