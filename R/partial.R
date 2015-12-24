#' Partial Dependence (One Variable)
#'
#' One- and two-dimensional partial dependence plots for objects of class
#' \code{mertree}.
#'
#' @param object A \code{mertree} object.
#' @param x.name Character string giving the name of the independent variable of
#'   interest.
#' @param n Integer giving the number of unique data points to use in
#'   computing the partial dependence values.
#' @param newdata An optional data frame.
#' @export
partial_1d <- function(object, x.name, n, newdata) {

  # Data frame
  .data <- if (missing(newdata)) eval(object$call$data) else newdata

  # Sorted unique values of the independent variable
  sux <- sort(unique(.data[[x.name]]))
  if (!missing(n)) {
    sux <- seq(from = min(sux), to = max(sux), length = n)
  }

  # Initialize vector to store partial dependence values
  pd <- numeric(length = length(sux))

  # For now, just use a for loop
  for (i in seq_len(length(sux))) {
    temp <- .data
    temp[[x.name]] <- sux[i]
    pd[i] <- mean(predict(object, newdata = temp))
  }

  # Return data frame of partial dependence values
  pd_df <- data.frame(x = sux, y = pd)
  names(pd_df) <- c(x.name, "y")
  pd_df

}


#' Partial Dependence (Two Variables)
#'
#' One- and two-dimensional partial dependence plots for objects of class
#' \code{mertree}.
#'
#' @param object A \code{mertree} object.
#' @param x1.name Character string giving the name of the first independent
#'   variable of interest.
#' @param x2.name Character string giving the name of the second independent
#'   variable of interest.
#' @param n1 Integer giving the number of unique data points to use in
#'   computing the partial dependence values for \code{x1.name}.
#' @param n2 Integer giving the number of unique data points to use in
#'   computing the partial dependence values for \code{x2.name}.
#' @param newdata An optional data frame.
#' @export
partial_2d <- function(object, x.name, n, newdata) {
  NULL
}
