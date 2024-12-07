% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dl2.R
\name{dl2}
\alias{dl2}
\title{Composite Indicator Algorithm}
\usage{
dl2(
  x,
  polarity,
  err,
  iterations,
  degrees,
  nfold = 5,
  trace = 0.5,
  use_normat = TRUE
)
}
\arguments{
\item{x}{A data frame of input variables.}

\item{polarity}{A numeraical vector indicating the polarity of variables (positive or negative).}

\item{err}{The acceptable error threshold for convergence.}

\item{iterations}{Maximum number of iterations. If the maximum number of iterations has been reached, increase the number of iterations.}

\item{degrees}{Maximum degree of interaction (Friedman’s mi). Default is 1, meaning build an additive model (i.e., no interaction terms).}

\item{nfold}{Number of folds for cross-validation in MARS. Cross-validation is an approach used to evaluate the robustness of predictive models. When the number of cross-validation folds (nfold) is greater than 1, the model is initially constructed using the full dataset. Subsequently, a series of nfold models are generated, with each model excluding a different subset of the data (out-of-fold data) to test the model's predictive performance. The R-squared statistic is calculated for each of these out-of-fold subsets, providing a measure of how well the model generalizes to unseen data. The cross-validated R-squared (CVRSq) is the mean of these R-squared values across all folds, offering a comprehensive assessment of the model's predictive accuracy. By default, the cross-validation process is repeated only once (ncross = 1), but can be repeated multiple times to reduce variability. To monitor the cross-validation process, the parameter trace = .5 can be used to provide output at each step. Additionally, detailed statistics, including cross-validation results, are available when the argument keepxy=TRUE is specified, or when using binomial or Poisson models with the glm argument.}

\item{trace}{Tracing in the earth function provides various levels of detail regarding the model-building process. The trace parameter allows users to control the verbosity of the output during execution. The default value of 0 produces no output. Setting trace to .3 will display information about the variance model (related to the varmod.method argument), while a value of .5 enables tracing of the cross-validation process (related to the nfold argument). Higher values provide more detailed output: a value of 1 gives an overview of the process, 2 traces the forward pass of the model, and 3 provides details of the pruning phase. For more comprehensive internal details, setting trace to 4 or 5 will output summaries and full matrices related to the model’s operations, respectively.}

\item{use_normat}{TRUE if data normalisation is necessary, FALSE if the entered data have just been normalised.}
}
\value{
A list with the following components:
\itemize{
\item \strong{normat}: The normalized data matrix after adjusting for variable polarity. Each variable is scaled between 0 and 1 depending on its polarity (positive or negative).
\item \strong{Weights}: A vector of weights for each variable, calculated using the MARS model. These weights are used to combine the variables into a composite indicator.
\item \strong{derror}: A numeric vector that tracks the error values for each iteration of the algorithm. The error is calculated as the difference between the composite indicator at each step and the target value.
\item \strong{frechet}: The initial Frechet index, which is a distance-based measure used to assess the proximity between the variables before any weights are applied.
\item \strong{corrgr}: A correlation plot (produced by the \code{corrgram} package) that visualizes the relationships between the normalized variables.
\item \strong{Comp.Indicator}: The final composite indicator after the iterative process. This is the weighted sum of the variables, which reflects the importance of each based on the calculated weights.
\item \strong{Comp.Indicator_1}: The composite indicator from the previous iteration, used to compare and update the indicator in subsequent iterations.
\item \strong{iteration}: The number of iterations the algorithm has performed before stopping, either due to reaching the error threshold or hitting the maximum number of iterations.
\item \strong{dist.in}: The initial distance matrix, which contains the squared differences between the variables, raised to the specified power (\code{degrees}).
\item \strong{dist.fin}: The final distance matrix after applying the calculated weights to the variables. This represents the final distance-based composite indicator.
}
}
\description{
This function computes a composite indicator using a series of steps
involving normalization, calculation of distances, weights through MARS,
and iterative error minimization.
}
\examples{
\dontrun{
  data <- data.frame(matrix(rnorm(500), ncol = 10))
  result <- dl2(data, polarity = c(1,2), err = 0.001, iterations = 50, degrees = 2,use_normat = TRUE)
  print(result$Comp.Indicator)
}

}
