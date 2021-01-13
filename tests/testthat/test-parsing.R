test_that("parsing works properly", {
  expr <- parse_safe(c("gamma", "", "alpha"))

  expect_identical(class(expr), "expression")
  expect_equal(expr, expression(gamma, NA, alpha))
  expect_type(expr, "expression")
})
