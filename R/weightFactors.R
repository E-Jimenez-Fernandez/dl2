#' Compute weights for variables using MARS.
#'
#' This function calculates the importance of each variable using Multivariate Adaptive Regression Splines (MARS).
#'
#' @param x A data frame containing the predictor variables.
#' @param Compind A vector representing the composite indicator.
#' @param degrees The degree for MARS modeling.
#' @param nfold Number of folds for cross-validation.
#' @param trace Controls the verbosity of the fitting process.
#' @return A list containing the computed weights, variable names, and importance measures.
#'
#' @importFrom dplyr mutate
#'
#' @examples
#' \dontrun{
#'   data <- data.frame(var1 = rnorm(10), var2 = rnorm(10))
#'   compind <- rnorm(10)
#'   weights <- weightFactors(data, compind, degrees = 2, fold = 5, trace = 0.5)
#'   print(weights[[1]])
#' }
#' @export
weightFactors <- function(x, Compind, degrees, nfold = 5, trace = .5) {
  m <- ncol(x)
  dat_os <- x
  x <- x %>% mutate(Cind = as.numeric(Compind))
  mymodel <- earth(Cind ~ ., data = x, degree = degrees, nfold = nfold, trace = trace)
  p2 <- evimp(mymodel, trim = FALSE)
  Variable <- row.names(p2)
  weights <- as.numeric(p2[, "rss"]) / 100
  Variable <- sub("-unused", "", Variable)

  if (all(is.na(weights)) || all(weights == 0)) {
    warning("All calculated weights are 0 or NA. Assigning equal weights.")
    weights <- rep(1 / length(weights), length(weights))
  } else {
    mini <- min(weights[weights > 0], na.rm = TRUE)
    for (j in 1:length(weights)) {
      if (is.na(weights[j])) {
        stop("Missing values were found in the weights.")
      } else if (weights[j] == 0) {
        weights[j] <- mini / length(weights)
      }
    }
  }

  res <- data.frame(t(weights))
  colnames(res) <- Variable
  res.ord <- as.numeric(res[colnames(dat_os)])
  important <- list(res.ord, colnames(dat_os), p2)

  return(important)
}
