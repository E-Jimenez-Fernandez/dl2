#' Normalize the data based on variable polarity.
#'
#' This function normalizes a data frame according to the specified polarity of the variables.
#' Positive polarity variables are normalized using the Min-Max method, while negative polarity
#' variables are normalized using the Max-Min method.
#'
#' @param x A data frame containing the variables to be normalized.
#' @param polarity A vector specifying the columns with positive polarity. All other columns are considered to have negative polarity
#' @param custom_max Optional vector of maximum values for each column. If NULL, the maximum is calculated automatically.
#' @param custom_min Optional vector of minimum values for each column. If NULL, the minimum is calculated automatically.
#'
#' @return A normalized data frame where each variable is scaled between 0 and 1.
#' @examples
#' \dontrun{
#'   data <- data.frame(var1 = rnorm(10), var2 = rnorm(10))
#'   normalized_data <- normalization(data, polarity = c(1), custom_max = NULL, custom_min = NULL)
#'   print(normalized_data)
#' }
#' @export
normalization <- function(x, polarity = NULL, custom_max = NULL, custom_min = NULL) {
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

  # Functions to normalize with custom min and max
  norm_minmax <- function(x, min_val, max_val) {
    (x - min_val) / (max_val - min_val)
  }

  norm_maxmin <- function(x, min_val, max_val) {
    (max_val - x) / (max_val - min_val)
  }

  # Use provided custom max/min or calculate them
  max_vals <- if (is.null(custom_max)) apply(x, 2, max) else custom_max
  min_vals <- if (is.null(custom_min)) apply(x, 2, min) else custom_min

  if (length(max_vals) != m || length(min_vals) != m) {
    stop("Length of custom_max and custom_min must match the number of columns in 'x'.")
  }

  # Normalize columns based on polarity and custom min/max
  for (j in pospol) {
    normdata[, j] <- norm_minmax(x[, j], min_vals[j], max_vals[j])
  }

  for (j in negpol) {
    normdata[, j] <- norm_maxmin(x[, j], min_vals[j], max_vals[j])
  }

  normdata <- as.data.frame(normdata)
  colnames(normdata) <- names_var
  rownames(normdata) <- names_regions
  return(normdata)
}
