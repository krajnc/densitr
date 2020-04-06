context("Loading tests")

string1 <- "data/0005/00/00050060.dpa"
string2  <- "00050060.dpa"


dp <- dpload(dp.file = system.file("extdata", "00010001.dpa", package = "densitr"))
dpl <- dpload(dp.directory = system.file("extdata", package = "densitr"))
dpl2 <- dpload(dp.directory = system.file("extdata", package = "densitr"), recursive = FALSE)

test_that("Extract dpa regex", {
  expect_match(extract_dpa_name(string1), "00050060")
  expect_match(extract_dpa_name(string2), "00050060")
})

test_that("Loading file vs folder", {
  expect_equal(length(dpl), 15)
  expect_equal(length(dpl2), 5)
  expect_is(dp, "dp")
  expect_is(dpl[[1]], "dp")
})

test_that("Combining dpas should always return df", {
  expect_true(is.data.frame(combine_footers(dpl2)))
  expect_true(is.data.frame(combine_data(dpl2)))
})

