context("Trim tests")


dp <- load_dpa(dpa.file = system.file("extdata", "00010001.dpa", package = "densiter"))
dpl <- load_dpa(dpa.directory = system.file("extdata", package = "densiter"))
dpl2 <- load_dpa(dpa.directory = system.file("extdata", package = "densiter"), recursive = FALSE)

dp.trim <- dtrim(dp)
dpl.trimmed <- dtriml(dpl)
dpl.trimmed2  <- dtrim_sl(dpl)
dpl.trimmed3  <- dtrim_sl(dpl, rreport = TRUE)
dpa.successful <- remove_trim_failures(dpa.trimmed)
dpa.failed <- separate_trim_failures(dpa.trimmed)

test_that("What trim returns", {
  expect_true(is.list(dpl.trimmed))
  expect_true(is.list(dpl.trimmed2))
  expect_is(dtrim(dp), "dpa")
  expect_is(dtrim_s(dp), "dpa")
  expect_is(dpl.trimmed[[1]], "dpa")
  expect_is(dpl.trimmed2[[1]], "dpa")
})

test_that("Manual removals", {
  expect_equal(length(dpa.successful), 9)
  expect_equal(length(dpa.failed$failures.end), 6)
  expect_equal(length(dpa.failed$failures.start), 0)
  expect_error(separate_trim_failures(dp))
  expect_error(separate_trim_failures(dpl.trimmed))
  expect_error(separate_trim_failures(dpl.trimmed2))
  expect_error(remove_trim_failures(dp))
  expect_error(remove_trim_failures(dpl.trimmed))
  expect_error(remove_trim_failures(dpl.trimmed2))
})
