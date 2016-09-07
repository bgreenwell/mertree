#' Terminal Node Assignment
#'
#' Assign observations to a terminal node.
#'
#' @param object An object that inherits from class \code{rpart}.
#' @param newdata An optional data frame in which to look for variables with
#'   which to predict. If omitted, the fitted values are used.
#' @param na.action The default action deletes all observations for which the
#'   response is missing, but keeps those in which one or more predictors are
#'   missing.
#' @param ... Additional optional arguments. At present, no optional arguments
#'   are used.
#'
#' @return A numeric vector containing the terminal node each observation
#'   belongs to.
#'
#' @rdname assign_node
#' @export
assign_leaf <- function(object, newdata, na.action, ...) {
  UseMethod("assign_leaf")
}


#' @rdname assign_leaf
#' @export
assign_leaf.mertree <- function(object, newdata, ...) {

  # Extract data if none are specified
  .data <- if (missing(newdata)) eval(object$call$data) else newdata

  # Dispatch on first argument
  treemisc::assign_leaf(object$tree_fit, newdata = .data, ...)

}
