context("Detection tests")

dp <- dpload(dp.file = system.file("extdata", "00010001.dpa", package = "densitr"))
dp.noend <-  dpload(dp.file = system.file("extdata", "subfolder/00050045.dpa", package = "densitr"))

dp.nostart  <- dp
dp.nostart$data  <-  dp$data[5000:nrow(dp$data),]

test_that("Should be a dpa object", {
  expect_error(dpdetect_s(123))
  expect_error(dpdetect_e(123))
})

test_that("Output should be numeric", {
  expect_true(is.numeric(dpdetect_s(dp)))
  expect_true(is.numeric(dpdetect_e(dp)))
})

test_that("Give warning when not detected", {
  expect_warning(dpdetect_e(dp.noend))
  expect_warning(dpdetect_s(dp.nostart))
})
