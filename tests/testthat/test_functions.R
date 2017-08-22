library(DoOR.functions)
library(DoOR.data)
load_door_data(nointeraction = TRUE)
context("correct output")

test_that("reset_sfr produces data with SFR value of 0", {
  door_response_matrix_SFRreset <- reset_sfr(door_response_matrix, "SFR")
  expect_that(all(door_response_matrix_SFRreset["SFR",] == 0, na.rm = TRUE), is_true())
})

test_that("door_norm rescales [0,1]", {
  expect_that(range(door_norm(123:321)), is_identical_to(range(0,1)))
})

test_that("calculate_model picks proper models", {
  expect_that(names(calculate_model(x = door_norm(1:10), y = door_norm(1:10))), matches("linear"))
})




