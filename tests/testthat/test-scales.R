source(testthat::test_path("fixtures", "data.R"))

# QC scales
test_that("scale_colour_qc() returns a Scale object", {
  s <- scale_colour_qc()
  expect_s3_class(s, "Scale")
})

test_that("scale_fill_qc() returns a Scale object", {
  expect_s3_class(scale_fill_qc(), "Scale")
})

test_that("scale_colour_qc() has all 6 flag values", {
  s      <- scale_colour_qc()
  s$train(as.character(1:6))
  mapped <- s$map(as.character(1:6))
  expect_length(mapped, 6L)
  expect_false(any(is.na(mapped)))
})

# Condition scales
test_that("scale_colour_condition() returns a Scale object", {
  expect_s3_class(scale_colour_condition(), "Scale")
})

test_that("scale_fill_condition() covers all five rating levels", {
  s <- scale_fill_condition()
  ratings <- c("Good", "Satisfactory", "Requires Attention", "Poor", "Critical")
  s$train(ratings)
  mapped <- s$map(ratings)
  expect_length(mapped, 5L)
  expect_false(any(is.na(mapped)))
})

# Tier scales
test_that("scale_colour_tier() covers Tier 3, Tier 2, Tier 1", {
  s <- scale_colour_tier()
  s$train(c("Tier 3", "Tier 2", "Tier 1"))
  mapped <- s$map(c("Tier 3", "Tier 2", "Tier 1"))
  expect_length(mapped, 3L)
})

# General flood palette
test_that("scale_colour_flood() returns a Scale object", {
  expect_s3_class(scale_colour_flood(), "Scale")
})
