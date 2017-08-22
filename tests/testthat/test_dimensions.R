library(DoOR.functions)
library(DoOR.data)
load_door_data(nointeraction = TRUE)
context("dimensionality & class of output")

test_that("model_responses creates a list", {
  expect_that(model_response(Or35a, plot = FALSE), is_a("list"))
})

test_that("model response output is numeric", {
  expect_that(model_response(Or35a)$model.response$merged_data, is_a("numeric"))
})

test_that("reset_sfr produces data.frame", {
  expect_that(reset_sfr(door_response_matrix, "SFR"), is_a("data.frame"))
})

test_that("reset_sfr output has same dimenstions as input", {
  expect_that(dim(reset_sfr(door_response_matrix, "SFR")), is_identical_to(dim(door_response_matrix)))
})

test_that("select_model produces a list", {
  studies <- names(ac3B)[c(7:8)]
  data_candidate <- ac3B[,c(7:8)]
  norm_data_candidate <- apply(data_candidate, 2, door_norm)
  expect_that(select_model(candidate = studies, data_candidate = norm_data_candidate, merged = FALSE), is_a("list"))

})

test_that("project_points produces proper output", {
  x <- door_norm(Or23a[,'Hallem.2004.EN'])
  y <- door_norm(Or23a[,'Hallem.2006.EN'])
  output <- project_points(x = x, y = y, plot = FALSE)
  expect_that(output, is_a("list"))
})


test_that("calculate_model returns a list", {
  x <- door_norm(Or35a[,6])
  y <- door_norm(Or35a[,9])
  expect_that(calculate_model(x, y, select.MD = door_default_values("select.MD")), is_a("list"))

})

test_that("update_database with permutation = TRUE updates all door data objects",{
  load_door_data(nointeraction = TRUE)
  tmp_a <- door_response_matrix
  tmp_b <- door_response_matrix_non_normalized
  tmp_c <- door_excluded_data

  expect_true(identical(tmp_a, door_response_matrix))
  expect_true(identical(tmp_b, door_response_matrix_non_normalized))
  expect_true(identical(tmp_c, door_excluded_data))

  remove_study("Montague.2011.EN")
  remove_study("Kreher.2008.EN")

  update_door_database(receptor="Or67b", permutation = TRUE)

  expect_false(identical(tmp_a, door_response_matrix))
  expect_false(identical(tmp_b, door_response_matrix_non_normalized))
  expect_false(identical(tmp_c, door_excluded_data))

})

test_that("update_database with permutation = FALSE updates all door data objects",{
  load_door_data(nointeraction = TRUE)
  tmp_a <- door_response_matrix
  tmp_b <- door_response_matrix_non_normalized
  tmp_c <- door_excluded_data

  expect_true(identical(tmp_a, door_response_matrix))
  expect_true(identical(tmp_b, door_response_matrix_non_normalized))
  expect_true(identical(tmp_c, door_excluded_data))

  remove_study("Montague.2011.EN")
  remove_study("Kreher.2008.EN")

  update_door_database(receptor="Or67b", permutation = FALSE)

  expect_false(identical(tmp_a, door_response_matrix))
  expect_false(identical(tmp_b, door_response_matrix_non_normalized))
  expect_false(identical(tmp_c, door_excluded_data))
})
