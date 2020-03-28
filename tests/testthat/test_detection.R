context("Detection tests")

dp <- load_dpa(dpa.file = system.file("extdata", "00010001.dpa", package = "densiter"))
dp.noend <-  load_dpa(dpa.file = system.file("extdata", "subfolder/00050045.dpa", package = "densiter"))

dp.nostart  <- dp
dp.nostart$data  <-  dp$data[5000:nrow(dp$data),]

test_that("Should be a dpa object", {
  expect_error(dpa_detect_start(123))
  expect_error(dpa_detect_end(123))
})

test_that("Output should be numeric", {
  expect_true(is.numeric(dpa_detect_start(dp)))
  expect_true(is.numeric(dpa_detect_end(dp)))
})

test_that("Give warning when not detected", {
  expect_warning(dpa_detect_end(dp.noend))
  expect_warning(dpa_detect_start(dp.nostart))
})
