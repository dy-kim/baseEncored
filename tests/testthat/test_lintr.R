if (requireNamespace("lintr", quietly = TRUE)) {
  context("Check lints")
  test_that("Package Style", {
    lintr::expect_lint_free()
  })
}
