#' Title  function creates a scatter plot given x vector and y vector
#'
#' @param x a vector of quantitative data
#' @param y a vector of quantitative data
#'
#' @export
#'
#' @return None
#' @examples scatterPlot(1:10, 1:10)

scatterPlot <- function(x, y){
  plot(x,y,
       pch = 21, bg = "blue",cex = 1.2,
       xlim = c(0, 1.1 * max(x)), ylim = c(0, 1.1 * max(y)))
}
