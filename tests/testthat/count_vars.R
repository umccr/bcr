context("test-counts")

test_that("count vars is integer", {
  expect_equal(read_count_file(system.file("extdata", "count_vars.txt", package = "woofr")), 13222)
})
