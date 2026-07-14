library(DoOR.functions)
library(DoOR.data)
load_door_data(nointeraction = TRUE)
context("correct output")

test_that("reset_sfr produces data with SFR value of 0", {
  door_response_matrix_SFRreset <-
    reset_sfr(door_response_matrix, "SFR")
  expect_true(all(door_response_matrix_SFRreset["SFR", ] == 0, na.rm = TRUE))
})

test_that("door_norm rescales [0,1]", {
  expect_identical(range(door_norm(123:321)), range(0, 1))
})

test_that("calculate_model picks proper models", {
  expect_match(names(calculate_model(
    x = door_norm(1:10), y = door_norm(1:10)
  )), "linear")
})
