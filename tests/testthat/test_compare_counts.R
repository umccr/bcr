context("test-counts")

count_vars <- read_count_file(system.file("extdata", "count_vars.txt", package = "woofr"))
eval_stats <- read_eval_file(system.file("extdata", "eval_stats.tsv", package = "woofr"))

test_that("count vars is integer", {
  expect_equal(count_vars, 13222)
  expect_equal(ncol(eval_stats), 18)
})

