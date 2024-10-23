#' Compute the Fréchet index for composite indicators.
#'
#' This function calculates the Fréchet index, which aggregates the distances of the variables.
#'
#' @param x A data frame of distances calculated from the input variables.
#' @param degrees The degree to which the distances are raised (typically used in calculateDistance).
#' @return A vector representing the Fréchet index for each observation.
#' @examples
#' \dontrun{
#'   distance_matrix <- matrix(rnorm(50), nrow = 10)
#'   frechet_index <- indexFrechet(distance_matrix, degrees = 2)
#'   print(frechet_index)
#' }
#' @export
indexFrechet <- function(x, degrees = 2) {
  iF <- (matrix(rowSums(x), ncol = 1))^(1 / degrees)
  colnames(iF) <- "Frechet.Index"
  return(iF)
}
