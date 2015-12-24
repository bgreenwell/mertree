context("formula")

test_that("formula functions work as expected", {

  # Example formula
  form <- y ~ x1 + x2 + x1 * x2 + (0 + 1 | id) + (0 + x1 | id)

  # Extract response name
  yname <- get_response_name(form)
  expect_identical(yname, "y")

  # Extract fixed effects portion of formula
  fixed <- get_fixed_formula(form)
  expect_identical(fixed, "x1 + x2 + x1 * x2")

  # Extract random effects portion of formula
  random <- get_random_formula(form)
  expect_identical(random, "(0 + 1 | id) + (0 + x1 | id)")

  # rpart and ctree formula
  tree_formula <- make_tree_formula(yname, fixed)
  expect_equal(tree_formula, y ~ x1 + x2 + x1 * x2)

  # lmer formula
  lmer_formula <- make_lmer_formula(yname, fixed, random)
  expect_equal(lmer_formula, form)

  # Just fit an intercept (used when tree contains no splits)
  expect_equal(make_lmer_formula("y", "1"), y ~ 1)

})
