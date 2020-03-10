context("test-graphic")

##the package is compose of mainly visualization functions that can't be conventionally tested.
##only a few functions calculating certain values are tested.

test_that("rotate works", {
  expect_equal(rotate(c(0, 100), theta=pi/6), c(-50.00000, 86.60254))
})

test_that("get_inv_col works", {
  expect_match(get_inv_col(25), 'black')
  expect_match(get_inv_col(10), 'white')
})

test_that("caltheta works", {
  expect_equal(caltheta(c(66, 66), pd=66, eye="right"), atan(-.5))
})


