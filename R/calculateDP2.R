#' Calculate the DP2 composite indicator.
#'
#' This function computes the composite indicator based on the weighted factors obtained from MARS.
#'
#' @param x A data frame containing the normalized variables.
#' @param xFactores A vector of weights for each variable.
#' @param iteration The current iteration number (for labeling purposes).
#' @return A list containing the composite indicator and the weighted matrix.
#' @examples
#' \dontrun{
#'   normalized_data <- data.frame(var1 = rnorm(10), var2 = rnorm(10))
#'   weights <- c(0.5, 0.5)
#'   dp2_result <- calculateDP2(normalized_data, weights)
#'   print(dp2_result[[1]])
#' }
#' @export
calculateDP2 <- function(x, xFactores, iteration = 1) {
  n <- dim(x)[1]
  m <- dim(x)[2]
  coefs <- matrix(xFactores, n, m, byrow = TRUE)
  mDP <- x * coefs
  DP2 <- sqrt(t(t(apply(mDP, MARGIN = 1, sum))))
  colnames(DP2) <- paste("p2distance", iteration, sep = ".")
  calcDP2 <- list(DP2, mDP)
  return(calcDP2)
}
