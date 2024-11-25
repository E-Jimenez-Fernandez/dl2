#' Calculate distances between variables.
#'
#' This function computes the distance between variables in a data frame by raising the differences to the specified power (degrees).
#'
#' @param x A data frame of numeric variables.
#' @param degrees The degree to which the differences are raised (e.g., Euclidean distance corresponds to degrees = 2).
#' @return A data frame where each element represents the distance raised to the specified degree.
#' @examples
#' \dontrun{
#'   data <- data.frame(var1 = rnorm(10), var2 = rnorm(10))
#'   distance_matrix <- calculateDistance(data, degrees = 2)
#'   print(distance_matrix)
#' }
#' @export
calculateDistance <- function(x, degrees = 2) {
  mDif.sqr <- x^degrees
  return(mDif.sqr)
}
