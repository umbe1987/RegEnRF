test_that("rerf output is correct", {
  x <- matrix(rnorm(100 * 20), 100, 20)
  y <- rnorm(100)
  mod <- rerf(x, y, 0.1)

  expect_s3_class(mod, "rerf")
})
