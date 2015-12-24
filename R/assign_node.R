
#' Terminal Node Assignment
#'
#' Assign each observation of the original data (or newdata) to a terminal
#' node. Based on a nice hack from stackoverflow.
#'
#' @rdname assign_node
#' @export
#'
#' @param object An object that inherits from class \code{rpart}.
#' @param newdata An optional data frame in which to look for variables with
#'   which to predict. If omitted, the fitted values are used.
#' @param na.action a function which indicates what should happen when the
#'   data contain \code{NA}s.
#' @param ... Additional optional arguments. At present, no optional arguments
#'   are used.
#'
#' @return A numeric vector containing the terminal node each observation
#'   belongs to.
assign_node <- function(object, newdata, ...) {
  UseMethod("assign_node")
}


#' @method assign_node rpart
#' @importFrom stats predict
assign_node.rpart <- function(object, newdata, na.action = na.pass, ...) {

  # Extract data if none are specified
  .data <- if (missing(newdata)) eval(object$call$data) else newdata

  # Replace fitted values with the corresponding node number
  object$frame$yval <- seq_len(NROW(object$frame))
  # object$frame$yval <- rownames(object$frame)  # as.numeric(rownames(object$frame))

  # Return node predictions
  unname(predict(object, newdata = .data, type = "vector",
                 na.action = na.action, ...))

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
