#' Terminal Node Assignment
#'
#' Assign observations to a terminal node. Based on a nice hack from
#' stackoverflow.
#'
#' @rdname assign_node
#' @export
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
assign_node <- function(object, newdata, ...) {
  UseMethod("assign_node")
}


#' @method assign_node rpart
<<<<<<< HEAD
#' @import rpart
#' @importFrom stats .checkMFClasses delete.response model.frame
assign_node.rpart <- function(object, newdata, na.action = na.rpart, ...) {

  # Use rpart internals to predict terminal node assignments
  if (missing(newdata)) {
    object$where
  } else {
    if (is.null(attr(newdata, "terms"))) {
      Terms <- delete.response(object$terms)
      newdata <- model.frame(Terms, newdata, na.action = na.action,
                             xlev = attr(object, "xlevels"))
      if (!is.null(cl <- attr(Terms, "dataClasses"))) {
        .checkMFClasses(cl, m = newdata, ordNotOK = TRUE)
      }
    }
    rpart:::pred.rpart(object, rpart:::rpart.matrix(newdata))
  }
=======
#' @importFrom stats predict
assign_node.rpart <- function(object, newdata, ...) {

  # Extract data if none are specified
  .data <- if (missing(newdata)) eval(object$call$data) else newdata

  # Replace fitted values with the corresponding node number
  object$frame$yval <- seq_len(NROW(object$frame))
  # object$frame$yval <- rownames(object$frame)  # as.numeric(rownames(object$frame))

  # Return node predictions
  unname(predict(object, newdata = .data, type = "vector", ...))
>>>>>>> c33a8555a64cb2a6652dd0a5d0196fdabe124d93

}


#' @method assign_node BinaryTree
assign_node.BinaryTree <- function(object, newdata, ...) {

  # Extract data if none are specified
  .data <- if (missing(newdata)) eval(object$call$data) else newdata

  # Use built-in slot function
  object@predict_response(newdata = .data, type = "node", ...)

}


#' @method assign_node mertree
assign_node.mertree <- function(object, newdata, ...) {

  # Extract data if none are specified
  .data <- if (missing(newdata)) eval(object$call$data) else newdata

  # Dispatch on first argument
  assign_node(object$tree_fit, newdata = .data, ...)

}
