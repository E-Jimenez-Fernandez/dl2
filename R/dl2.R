#' Composite Indicator Algorithm
#'
#' This function computes a composite indicator using a series of steps
#' involving normalization, calculation of distances, weights through MARS,
#' and iterative error minimization.
#'
#' @param x A data frame of input variables.
#' @param polarity A numeraical vector indicating the polarity of variables (positive or negative).
#' @param err The acceptable error threshold for convergence.
#' @param iterations Maximum number of iterations. If the maximum number of iterations has been reached, increase the number of iterations.
#' @param degrees Maximum degree of interaction (Friedman’s mi). Default is 1, meaning build an additive model (i.e., no interaction terms).
#' @param nfold Number of folds for cross-validation in MARS. Cross-validation is an approach used to evaluate the robustness of predictive models. When the number of cross-validation folds (nfold) is greater than 1, the model is initially constructed using the full dataset. Subsequently, a series of nfold models are generated, with each model excluding a different subset of the data (out-of-fold data) to test the model's predictive performance. The R-squared statistic is calculated for each of these out-of-fold subsets, providing a measure of how well the model generalizes to unseen data. The cross-validated R-squared (CVRSq) is the mean of these R-squared values across all folds, offering a comprehensive assessment of the model's predictive accuracy. By default, the cross-validation process is repeated only once (ncross = 1), but can be repeated multiple times to reduce variability. To monitor the cross-validation process, the parameter trace = .5 can be used to provide output at each step. Additionally, detailed statistics, including cross-validation results, are available when the argument keepxy=TRUE is specified, or when using binomial or Poisson models with the glm argument.
#' @param trace Tracing in the earth function provides various levels of detail regarding the model-building process. The trace parameter allows users to control the verbosity of the output during execution. The default value of 0 produces no output. Setting trace to .3 will display information about the variance model (related to the varmod.method argument), while a value of .5 enables tracing of the cross-validation process (related to the nfold argument). Higher values provide more detailed output: a value of 1 gives an overview of the process, 2 traces the forward pass of the model, and 3 provides details of the pruning phase. For more comprehensive internal details, setting trace to 4 or 5 will output summaries and full matrices related to the model’s operations, respectively.
#' @param use_normat TRUE if data normalisation is necessary, FALSE if the entered data have just been normalised.
#'
#' @return A list with the following components:
#'
#' - **normat**: The normalized data matrix after adjusting for variable polarity. Each variable is scaled between 0 and 1 depending on its polarity (positive or negative).
#'
#' - **Weights**: A vector of weights for each variable, calculated using the MARS model. These weights are used to combine the variables into a composite indicator.
#'
#' - **derror**: A numeric vector that tracks the error values for each iteration of the algorithm. The error is calculated as the difference between the composite indicator at each step and the target value.
#'
#' - **frechet**: The initial Frechet index, which is a distance-based measure used to assess the proximity between the variables before any weights are applied.
#'
#' - **corrgr**: A correlation plot (produced by the `corrgram` package) that visualizes the relationships between the normalized variables.
#'
#' - **Comp.Indicator**: The final composite indicator after the iterative process. This is the weighted sum of the variables, which reflects the importance of each based on the calculated weights.
#'
#' - **Comp.Indicator_1**: The composite indicator from the previous iteration, used to compare and update the indicator in subsequent iterations.
#'
#' - **iteration**: The number of iterations the algorithm has performed before stopping, either due to reaching the error threshold or hitting the maximum number of iterations.
#'
#' - **dist.in**: The initial distance matrix, which contains the squared differences between the variables, raised to the specified power (`degrees`).
#'
#' - **dist.fin**: The final distance matrix after applying the calculated weights to the variables. This represents the final distance-based composite indicator.
#'
#' @examples
#' \dontrun{
#'   data <- data.frame(matrix(rnorm(500), ncol = 10))
#'   result <- dl2(data, polarity = c(1,2), err = 0.001, iterations = 50, degrees = 2,use_normat = TRUE)
#'   print(result$Comp.Indicator)
#' }
#'
#' @import dplyr
#' @import earth
#' @import corrgram
#' @export


dl2 <- function(x, polarity, err, iterations, degrees, nfold = 5, trace = .5, use_normat = TRUE) {

  # Ensure input is a data frame
  if (!is.data.frame(x)) {
    warning("The argument 'x' must be a data.frame object.")
    x <- as.data.frame(x)
  }

  # Conditionally normalize the data based on polarity
  if (use_normat) {
    normat <- normalization(x, polarity)
  } else {
    normat <- x
  }

  # Compute correlation between variables
  corrgr <- corrgram(normat, order=TRUE, lower.panel=panel.shade,
                     upper.panel=panel.pie, text.panel=panel.txt,
                     main="Correlation between features")

  # Calculate distances (differences raised to the specified degree)
  mDif <- calculateDistance(normat)

  # Compute the initial Frechet index
  dps <- indexFrechet(mDif)
  dp2_aux <- dps
  iteration <- 1
  error.d <- numeric()

  # Begin iterative algorithm to minimize error and adjust weights
  repeat {
    print(paste("Iteration", iteration))

    # Calculate weights using MARS and variable importance
    weigths <- weightFactors(normat, dps, degrees, nfold = nfold, trace = trace)[[1]]

    # Recalculate DP2 composite indicator
    calcDP2 <- calculateDP2(mDif, weigths)
    d2_ite <- calcDP2[[1]]

    # Calculate the error (sum of squared differences)
    error <- sum((dps - d2_ite)^2)
    error.d[iteration] <- error

    # Stop if the error is below the threshold or the maximum iterations is reached
    if ((error < err) || (iteration >= iterations)) {
      break
    }

    iteration <- iteration + 1
    dps <- d2_ite
  }

  # Return the results
  invisible(return(list(
    normat = if (use_normat) normat else NULL, # Only return normat if used
    Weights =  weigths,
    derror = error.d,
    frechet = dp2_aux,
    corrgr  = corrgr,
    Comp.Indicator = d2_ite,
    Comp.Indicator_1 = dps,
    iteration = iteration,
    dist.in = mDif,
    dist.fin = calcDP2[[2]]
  )))
}


