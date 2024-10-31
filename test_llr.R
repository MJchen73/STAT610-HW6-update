# "Check local linear regression function"
source("llr_functions.R")

n = 15
## a very simple regression model
x = rnorm(n)
y = rnorm(x + rnorm(n))
z = seq(-1, 1, length.out = 100)

# Test cases below:
# Check if "llr" output has correct length:
test_that("llr output has correct length", {
    expect_equal(length(llr(x, y, z, omega = 1)), length(z))
})

# Check if "make_weight_matrix" function works:
test_that("make_weight_matrix works on simple cases", {
  ## check that the output is a diagonal matrix, 
  ## that all the elements are positive, 
  ## and that the weights are correct in simple cases
  z = 0
  omega = 1
  Wz = make_weight_matrix(z, x, omega)
  expect_equal(dim(Wz), c(n, n))  # Check if Wz is n x n matrix
  expect_true(all(diag(Wz) >= 0))  # Check if all diagonal elements are non-negative
  expect_equal(Wz[upper.tri(Wz)], rep(0,n*(n-1)/2))  # Ensure the matrix is diagonal
  expect_equal(Wz[lower.tri(Wz)], rep(0,n*(n-1)/2))
})

# Check if "make_predictor_matrix" function works:
test_that("make_predictor_matrix works on simple cases", {
  X = make_predictor_matrix(x)
  expect_equal(dim(X), c(n, 2))  # X should have n rows and 2 columns
  expect_equal(X[, 1], rep(1, n))  # The first column should be all ones
  expect_equal(X[, 2], x)  # The second column should be equal to x
})