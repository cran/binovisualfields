context("test-calculation")


test_that("rotate", {
  expect_error(rotate(5, theta=0),"'xy' must be a two-element vector")
  expect_equal(rotate(c(100, 0), theta=0), c(100, 0))
  expect_equal(rotate(c(100, 0), theta=pi/6), c(86.60254, 50.00000))
})

test_that("get_inv_col", {
  expect_match(get_inv_col(25), 'black')
  expect_match(get_inv_col(10), 'white')
})

test_that("caltheta", {
  expect_equal(caltheta(c(66, 66), pd=66, eye="right"), atan(-.5))
})

test_that("ivfcal", {
  expect_error(binovfcal(matrix(rep(30, 80), ncol=10, byrow = TRUE), matrix(rep(30, 80), ncol=10, byrow = TRUE),seq(300, 500, 10), .1, -.1),
               "the value of either pd or gender has to be provided")
  expect_error(binovfcal(matrix(rep(30, 80), ncol=10, byrow = TRUE), matrix(rep(30, 80), ncol=10, byrow = TRUE),seq(300, 500, 10), .1, -.1, gender='m'),
               "is not TRUE")
})

