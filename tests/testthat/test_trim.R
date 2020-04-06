context("Trim tests")


dp <- dpload(dp.file = system.file("extdata", "00010001.dpa", package = "densitr"))
dpl <- dpload(dp.directory = system.file("extdata", package = "densitr"))
dpl2 <- dpload(dp.directory = system.file("extdata", package = "densitr"), recursive = FALSE)

dp.trim <- dptrim(dp)
dpl.trimmed <- dptriml(dpl)
dpl.trimmed2  <- dptriml_s(dpl)
dpl.trimmed3  <- dptriml(dpl, rreport = TRUE)
dp.successful <- remove_trim_failures(dpl.trimmed3)
dp.failed <- separate_trim_failures(dpl.trimmed3)

test_that("What trim returns", {
  expect_true(is.list(dpl.trimmed))
  expect_true(is.list(dpl.trimmed2))
  expect_is(dptrim(dp), "dp")
  expect_is(dptrim_s(dp), "dp")
  expect_is(dpl.trimmed[[1]], "dp")
  expect_is(dpl.trimmed2[[1]], "dp")
})

test_that("Manual removals", {
  expect_equal(length(dp.successful), 9)
  expect_equal(length(dp.failed$failures.end), 6)
  expect_equal(length(dp.failed$failures.start), 0)
  expect_error(separate_trim_failures(dp))
  expect_error(separate_trim_failures(dpl.trimmed))
  expect_error(separate_trim_failures(dpl.trimmed2))
  expect_error(remove_trim_failures(dp))
  expect_error(remove_trim_failures(dpl.trimmed))
  expect_error(remove_trim_failures(dpl.trimmed2))
})
