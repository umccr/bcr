context("test-comparison")

snv_count <- read_count_file(system.file("extdata", "snv/count_vars.txt", package = "woofr"))
snv_eval <- read_snv_eval_file(system.file("extdata", "snv/eval_stats.tsv", package = "woofr"))
sv_eval <- read_sv_eval_file(system.file("extdata", "sv/eval_metrics.tsv", package = "woofr"))
sv_fpfn <- read_sv_fpfn_file(system.file("extdata", "sv/fpfn.tsv", package = "woofr"))

test_that("snv count is correct format", {
  expect_equal(snv_count, 13222)
})

test_that("snv eval is correct format", {
  expect_equal(ncol(snv_eval), 18)
})

test_that("sv eval is correct format", {
  expect_equal(ncol(sv_eval), 10)
})

test_that("sv fpfn is correct format", {
  expect_equal(ncol(sv_fpfn), 6)
})
