#' Normalize the data based on variable polarity.
#'
#' This function normalizes a data frame according to the specified polarity of the variables.
#' Positive polarity variables are normalized using the Min-Max method, while negative polarity
#' variables are normalized using the Max-Min method.
#'
#' @param x A data frame containing the variables to be normalized.
#' @param polarity A vector specifying the columns with positive polarity. All other columns are considered to have negative polarity.
#' @return A normalized data frame where each variable is scaled between 0 and 1.
#' @examples
#' \dontrun{
#'   data <- data.frame(var1 = rnorm(10), var2 = rnorm(10))
#'   normalized_data <- normalization(data, polarity = c(1))
#'   print(normalized_data)
#' }
#' @export
normalization <- function(x, polarity = NULL) {
  names_var <- colnames(x)
  names_regions <- rownames(x)
  m <- dim(x)[2]
  columns <- c(1:m)
  pospol <- polarity
  if (!is.null(polarity)) {
    negpol <- columns[-pospol]
  } else {
    negpol <- columns
  }

  normdata <- matrix(0, ncol = ncol(x), nrow = nrow(x))

  norm_minmax <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }

  norm_maxmin <- function(x) {
    (max(x) - x) / (max(x) - min(x))
  }

  for (j in pospol) {
    normdata[, j] <- norm_minmax(x[, j])
  }

  for (j in negpol) {
    normdata[, j] <- norm_maxmin(x[, j])
  }

  normdata <- as.data.frame(normdata)
  colnames(normdata) <- names_var
  rownames(normdata) <- names_regions
  return(normdata)
}
