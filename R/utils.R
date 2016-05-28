#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


#' Number of Leaves/Terminal Nodes
#'
#' Compute the number of leaves (i.e., the number of temrinal nodes) of a tree.
#'
#' @param object An object of class \code{"rpart"} or \code{"BinaryTree"}.
#'
#' @return Returns \code{TRUE} if \code{object} does not contain any splits and
#' \code{FALSE} otherwise.
#'
#' @export
nleaves <- function(object) {
  UseMethod("nleaves")
}


#' @export
#' @rdname nleaves
nleaves.rpart <- function(object, ...) {
  length(unique(object$where))
}


#' @export
#' @rdname nleaves
nleaves.BinaryTree <- function(object, ...) {
  length(unique(where(object)))
}
