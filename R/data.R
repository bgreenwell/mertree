#' Simulated Data
#'
#' The data are described in [REF1] and (originally) in[REF2].
#'
#' \itemize{
#'   \item \code{subject} Subject ID.
#'   \item \code{x1} Baseline covariate: \eqn{x_i \sim U\left(0, 1\right)}.
#'   \item \code{x2} Baseline covariate: \eqn{x_i \sim U\left(0, 1\right)}.
#'   \item \code{x3} Baseline covariate: \eqn{x_i \sim U\left(0, 1\right)}.
#'   \item \code{x4} Baseline covariate: \eqn{x_i \sim U\left(0, 1\right)}.
#'   \item \code{x5} Baseline covariate: \eqn{x_i \sim U\left(0, 1\right)}.
#'   \item \code{x6} Time-dependent covariate: \eqn{x_i \sim U\left(0, 1\right)}.
#'   \item \code{time} Time variable: 1 to 5.
#'   \item \code{y} Response variable.
#' }
#' @details
#' The data, taken from Friedman, Grosse, and Stuetzle (1983) and Friedman (1991), were simulated from the function
#' \deqn{f\left(t, \boldsymbol{x}\right) = 10\sin\left(x_1 x_2 \pi\right) + 20\left(x_3 - 1/2\right)^2 + 5x_6 + 10t + \epsilon},
#' where \eqn{t = 1, 2, \dots, 5}, and \eqn{x_i \sim U\left(0, 1\right)} for \eqn{i = 1, 2 \dots 6}. The measurement was 
#' generated from a 5-dimensional Gaussian distribution with zero mean and a variance-covariance matrix with 4 along the diagonal
#' and 0.8 in the off-diagonal.
#' @docType data
#' @keywords datasets
#' @format A data frame with 500 rows and 9 variables
#' @name simd
#' @source
#' [REF1]
#' [REF2]
NULL
