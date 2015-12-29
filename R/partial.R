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
#' @param ... Additional optional arguments passed onto \code{aaply}.
#' @importFrom plyr laply
#' @export
partial_1d <- function(object, x.name, n, newdata, ...) {
  
  stopifnot(inherits(object, "mertree"))
  
  # Data frame
  .data <- if (missing(newdata)) eval(object$call$data) else newdata
  
  # Sorted unique values of the independent variable
  sux <- sort(unique(.data[[x.name]]))
  if (!missing(n)) {
    sux <- seq(from = min(sux), to = max(sux), length = n)
  }
  
  # Compute average prediction for each unique value
  pd <- laply(sux, .fun = function(x) {
    temp <- .data
    temp[[x.name]] <- x
    mean(predict(object, newdata = temp))
  }, ...)

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
#' @param ... Additional optional arguments passed onto \code{aaply}.
#' @importFrom plyr aaply
#' @export
partial_2d <- function(object, x1.name, x2.name, n1, n2, newdata, ...) {
  
  stopifnot(inherits(object, "mertree"))
  
  # Data frame
  .data <- if (missing(newdata)) eval(object$call$data) else newdata
  
  # Sorted unique values of the first independent variable
  sux1 <- sort(unique(.data[[x1.name]]))
  if (!missing(n1)) {
    sux1 <- seq(from = min(sux1), to = max(sux1), length = n1)
  }
  
  # Sorted unique values of the second independent variable
  sux2 <- sort(unique(.data[[x2.name]]))
  if (!missing(n2)) {
    sux2 <- seq(from = min(sux2), to = max(sux2), length = n2)
  }
  
  # Data frame of unique combinations
  xgrid <- expand.grid("x1" = sux1, "x2" = sux2)
  
  # Compute average prediction for each unique value
  pd_df <- adply(xgrid, .margins = 1, .fun = function(x) {
    temp <- .data
    temp[[x1.name]] <- x[[1]]
    temp[[x2.name]] <- x[[2]]
    mean(predict(object, newdata = temp))
  }, ...)

  # Return data frame of partial dependence values
  names(pd_df) <- c(x1.name, x2.name, "y")
  pd_df
  
}
