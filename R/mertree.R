#' Regression Trees for Longitudinal and Panel Data
#'
#' Fits an unbiased regression tree to longitudinal or panel data by iterating
#' back and forth between a conditional inference regression tree to capture
#' complex interactions and nonlinear relationaships and a linear mixed-effects
#' model to capture complex correlation structure.
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
#' @param tree.control List of control parameters for \code{\link{ctree}} or
#'   \code{\link{rpart}}.
#' @param tol The desired accuracy (convergence tolerance). Default is
#'   \code{0.001)}
#' @param maxiter Integer specifying the maximum number of iterations. Default
#'   is \code{1000}.
#' @param do.trace Logical indicating whether or not to print trace information.
#'
#' @importFrom lme4 lmer lmerControl
#' @importFrom party ctree ctree_control where
#' @importFrom rpart prune rpart rpart.control
#' @importFrom stats logLik predict
#'
#' @export
#'
#' @examples
#' data(Orthodont, package = "nlme")
#' fm <- mertree(distance ~ age + Sex + (1 | Subject), data = Orthodont)
#' fm2 <- mertree(distance ~ age + Sex + (1 | Subject), data = Orthodont,
#'                unbiased = FALSE, tree.control = rpart.control(cp = 0))
mertree <- function (formula, data, unbiased = TRUE, initial_re, REML = TRUE,
                     lmer.control = lmerControl(calc.derivs = FALSE),
                     tree.control = if (unbiased) ctree_control(mincriterion = 0.95) else rpart.control(),
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

    if (do.trace) {
      cat("  1. fitting tree...", "\n")
    }

#     form <- make_tree_formula("adj_response_values", fixed = fixed_formula)
#     print(form)
#     print(tree.control)

    # Fit a conditional inference tree
    if (unbiased) {
      tree_fit <- ctree(make_tree_formula("adj_response_values",
                                          fixed = fixed_formula),
                        data = newdata, controls = tree.control)
    }
    # Fit a CART-like regression tree
    else {
      if (cv) {
        temp <- rpart(make_tree_formula("adj_response_values",
                                        fixed = fixed_formula),
                      data = newdata, control = tree.control)
        opt <- temp$cptable[which.min(temp$cptable[, "xerror"]), "CP"]
        tree_fit <- prune(temp, cp = opt)
      } else {
        tree_fit <- rpart(make_tree_formula("adj_response_values",
                                            fixed = fixed_formula),
                          data = newdata, control = tree.control(xval = 0))
      }

    }

    # Add terminal node indicator variable
    .where <- if (unbiased) where(tree_fit) else tree_fit$where
    newdata[["terminal_node"]] <- as.factor(.where)


    ############################################################################
    # Linear mixed-effects model
    ############################################################################

    if (do.trace) {
      cat("  2. fitting mixed-effects model...", "\n\n")
    }

    # If the tree is a root (i.e., has no splits), then just fit an intercept
    if (min(.where) == max(.where)) {
      lmer_fit <- lmer(make_lmer_formula(response_name, fixed = "1"),
                       data = newdata, REML = REML, control = lmer.control)
    }
    # Otherwise, fit an linear mixed-effects model using a factor for terminal
    # node indicator as the fixed effects
    else {
      lmer_fit <- lmer(make_lmer_formula(response_name, fixed = "terminal_node",
                                         random = random_formula),
                       data = newdata, REML = REML, control = lmer.control)
    }

    # Update loop control variables
    iter <- iter + 1
    new_logLik <- logLik(lmer_fit)
    continue_condition <- (new_logLik - old_logLik) > tol & iter < maxiter
    old_logLik <- new_logLik

    # Update adjusted response values
    adj_response_values <- response_values -
      (predict(lmer_fit, re.form = NULL) - predict(lmer_fit, re.form = NA))
       # all random effects (XB + Zb)      # no random effects (XB)

  }

  # Print warning message about terminal node means
  warning("terminal node estimates are incorrect")

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
#' @export
varimp <- function(object, ...) {
  stopifnot(inherits(object, "mertree"))
  if (inherits(object$tree_fit, "rpart")) {
    object$tree_fit$variable.importance
  } else {
    stop(paste("mertree variable importance scores are not",
               "availble when unbiased = TRUE"))
  }
}


#' @method plot mertree
#' @importFrom graphics plot
#' @importFrom rpart.plot prp
#' @export
plot.mertree <- function(object, ...) {
  if (inherits(object$tree_fit, "rpart")) {
    prp(object$tree_fit, ...)
  } else {
    plot(object$tree_fit, ...)
  }
}


#' @method text mertree
#' @importFrom graphics text
#' @export
text.mertree <- function(object, ...) {
  text(object$tree_fit, ...)
}


#' @method print mertree
#' @export
print.mertree <- function(object, ...) {
  print(object$lmer_fit)
}


#' @method summary mertree
#' @export
summary.mertree <- function(object, ...) {
  summary(object$lmer_fit)
}


#' @method confint mertree
#' @importFrom stats confint
#' @export
confint.mertree <- function(object, ...) {
  confint(object$lmer_fit, ...)
}


#' @method predict mertree
#' @importFrom stats predict
#' @export
predict.mertree <- function(object, ...) {
  # FIXME: There's got to be a simpler way to accomplish this!
  x <- data.frame(node = assign_node(object, ...))
  x$id <- seq_len(nrow(x))
  y <- object$adj_node_means
  z <- merge(x, y, by = "node")
  z[order(z$id), ]$adjy
}
