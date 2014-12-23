
setwd ("C:/Users/Administrator/Desktop/Newton Method/")
source ("Newton.R")
library('testthat')

#test when there are no solutions
test_that('No solution', {
  A <- Newton (f=function(x){x^2+2}, 1, 0.01, 100, 0.00001, 6)  
  expect_that (nrow(A), equals(101))
})

#test when approaching the solution from right
test_that("Approach from right", {
  A <- Newton (f=function(x){x^2}, 1, 0.01, 100, 0.00001, 6)
  expect_that (abs(A[nrow(A),3]), equals(0, tolerance = 0.01))
})

#test when approaching the solution from left
test_that("Approach from left", {
  A <- Newton (f=function(x){x^2}, -1, 0.01, 100, 0.00001, 6)
  expect_that (abs(A[nrow(A),3]), equals(0, tolerance = 0.01))
})

#test when x0 is the solution
test_that("x0 is the solution",{
  A <- Newton (f=function(x){x^2}, 0, 0.01, 100, 0.00001, 6)
  expect_that (nrow(A), equals(1))
})