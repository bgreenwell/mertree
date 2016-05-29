#' Regression Trees for Longitudinal and Clustered Data
#'
#' Fits an unbiased regression tree to longitudinal or clustered data by
#' iterating back and forth between a conditional inference regression tree to
#' capture complex interactions and nonlinear relationaships and a linear
#' mixed-effects model to capture complex correlation structure.
#'
#' @param formula An appropriate \code{\link{lmer}}-style formula.
#' @param data An optional data frame containing the variables named in
#'   \code{formula}.
#' @param unbiased Logical indicating whether or not to use a conditional
#'   inference tree. Default is \code{TRUE}.
#' @param initial_re Numeric vector containing the initial values for the random
#'   effects. If omitted then defaults to zero.
#' @param REML Logical indicating whether or not the estimates should be chosen
#'   to optimize the REML criterion (as opposed to the log-likelihood).
#' @param lmer.control List of control parameters for \code{\link{lmer}}.
#' @param lmer.verbose Integer specifying the verbosity of output printed during
#'   the call to \code{lmer}. If > 0, verbose output is generated during the
#'   optimization of the parameter estimates. If > 1, verbose output is
#'   generated during the individual PIRLS steps. Default is \code{0L} meaning
#'   to supress such output.
#' @param tree.control List of control parameters for \code{\link{ctree}} or
#'   \code{\link{rpart}}.
#' @param cv Logical indicating whether or not to prune each tree based on
#'   cross-validations. Only used when \code{unbiased = FALSE}. Default is
#'   \code{TRUE}.
#' @param tol The desired accuracy (convergence tolerance). Default is
#'   \code{0.001)}
#' @param maxiter Integer specifying the maximum number of iterations. Default
#'   is \code{1000}.
#' @param do.trace Logical indicating whether or not to print trace information.
#'
#' @importFrom lme4 lmer lmerControl
#' @importFrom rpart rpart rpart.control prune
#' @importFrom party ctree ctree_control where
#' @importFrom stats logLik predict
#'
#' @export
mertree <- function (formula, data, unbiased = TRUE, initial_re, REML = TRUE,
                     lmer.control = lmerControl(calc.derivs = FALSE),
                     lmer.verbose = 0L,
                     tree.control = if (unbiased) {
                       ctree_control()
                     } else {
                       rpart.control()
                     },
                     cv = TRUE, tol = 0.001, maxiter = 100L, do.trace = FALSE) {

  # Initialize random effects estimate
  if (missing(initial_re)) {
    initial_re <- numeric(length = dim(data)[1L])
  }

  # Extract formula components
  response_name <- get_response_name(formula)
  fixed_formula <- get_fixed_formula(formula)
  random_formula <- get_random_formula(formula)

  # Vector of response values and adjusted response values
  response_values <- data[[response_name]]
  adj_response_values <- response_values - initial_re

  # Initialize loop control variables
  continue_condition <- TRUE
  iter <- 0
  old_logLik <- -Inf

  # Copy of original data
  newdata <- data

  # Iterate back and forth between a conditional inference tree and a linear
  # mixed-effects model
  while (continue_condition) {

    if (do.trace) {
      cat(paste0("iter ", iter + 1, ":"), "\n")
    }

    # Add column of adjusted response values
    newdata[["adj_response_values"]] <- adj_response_values

    ############################################################################
    # Regression tree
    ############################################################################

    # Print trace information
    if (do.trace) {
      cat("  1. fitting tree...", "\n")
    }

    # Tree formula
    tform <- make_tree_formula("adj_response_values", fixed = fixed_formula)

    # Fit a conditional inference tree
    if (unbiased) {
      tree_fit <- ctree(tform, data = newdata, controls = tree.control)
    }
    # Fit a CART-like regression tree
    else {
      if (cv) {
        temp <- rpart(tform, data = newdata, control = tree.control)
        opt <- temp$cptable[which.min(temp$cptable[, "xerror"]), "CP"]
        tree_fit <- prune(temp, cp = opt)
      } else {
        tree_fit <- rpart(tform, data = newdata,
                          control = tree.control(xval = 0))
      }

    }

    # Print trace information
    if (do.trace) {
      cat("     - nleaves:", nleaves(tree_fit), "\n\n")
    }

    # Add terminal node indicator variable
    .where <- if (unbiased) where(tree_fit) else tree_fit$where
    newdata[["terminal_node"]] <- as.factor(.where)


    ############################################################################
    # Linear mixed-effects model
    ############################################################################

    # Print trace information
    if (do.trace) {
      cat("  2. fitting mixed-effects model...", "\n")
    }

    # If the tree is a root (i.e., has no splits), then just fit an intercept
    if (min(.where) == max(.where)) {
      lmer_fit <- lmer(make_lmer_formula(response_name, fixed = "1"),
                       data = newdata, REML = REML, control = lmer.control,
                       verbose = lmer.verbose)
    }
    # Otherwise, fit a linear mixed-effects model using terminal node indicator
    # as the sole fixed effects term
    else {
      lmer_fit <- lmer(make_lmer_formula(response_name, fixed = "terminal_node",
                                         random = random_formula),
                       data = newdata, REML = REML, control = lmer.control,
                       verbose = lmer.verbose)
    }

    # Update loop control variables and print trace information
    iter <- iter + 1
    new_logLik <- logLik(lmer_fit)
    if (do.trace) {
      cat("     - old logLik:", old_logLik, "\n")
      cat("     - new logLik:", new_logLik, "\n")
      cat("     - |difference|:", abs(new_logLik - old_logLik), "\n\n")
    }

    continue_condition <- abs(new_logLik - old_logLik) > tol & iter < maxiter
    old_logLik <- new_logLik

    # Update adjusted response values
    adj_response_values <- response_values -
      (predict(lmer_fit, re.form = NULL) - predict(lmer_fit, re.form = NA))
       # all random effects (XB + Zb)      # no random effects (XB)

  }

  # Matched call
  mcall <- match.call()

  # Matrix of node assignments and adjusted means
  adj_node_means <- unique(cbind("node" = .where,
                                 "adjy" = predict(lmer_fit, re.form = NA)))
  rownames(adj_node_means) <- NULL

  # Return classed list of results
  res <- list("tree_fit" = tree_fit,
              "lmer_fit" = lmer_fit,
              "iter" = iter,
              "adj_node_means" = adj_node_means,
              "call" = mcall)
  class(res) <- "mertree"
  res

}


#' Variable Importance Scores
#'
#' Variable importance scores for \code{"mertree"} objects.
#'
#' @param object An object that inherits from class \code{"mertree"}.
#' @param ... Additional optional arguments. At present, no optional arguments
#'   are used.
#' @export
varimp <- function(object, ...) {
  stopifnot(inherits(object, "mertree"))
  if (inherits(object$tree_fit, "rpart")) {
    object$tree_fit$variable.importance
  } else {
    stop(paste("Variable importance is not available",
               "for unbiased regression trees."))
  }
}


#' @importFrom graphics plot
#' @importFrom partykit as.party
#' @export
plot.mertree <- function(x, ...) {
  if (inherits(x$tree_fit, "rpart")) {
    plot(as.party(x$tree_fit, ...))
  } else {
    plot(x$tree_fit, ...)
  }
}


#' @importFrom graphics text
#' @export
text.mertree <- function(x, ...) {
  text(x$tree_fit, ...)
}


#' @export
print.mertree <- function(x, ...) {
  print(x$lmer_fit)
}


#' @export
summary.mertree <- function(object, ...) {
  summary(object$lmer_fit)
}


#' @importFrom stats confint
#' @export
confint.mertree <- function(object, ...) {
  confint(object$lmer_fit, ...)
}


#' @importFrom stats predict
#' @export
predict.mertree <- function(object, ...) {
  map <- object$adj_node_means  # unique key, value pairs
  values <- assign_node(object, ...)  # new values to map
  unname(setNames(map[, "adjy"], map[, "node"])[as.character(values)])  # map new values
}
