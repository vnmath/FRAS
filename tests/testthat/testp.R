context("Test FRAS package")
library(testthat)
library(dplyr)
library(maps)

setwd(system.file("extdata", package = "FRAS"))
# test make_filename
expect_identical(make_filename(2013),
                 'accident_2013.csv.bz2')
# test fars_read
test_that("test fars_read() runs well", {
  expect_is(fars_read("accident_2013.csv.bz2"), "tbl_df")
  expect_error(fars_read("accident_2020.csv.bz2"))
})

# test fars_summarize_years

test_that("test fars_summarize_years() runs well", {
  expect_is(fars_summarize_years(2013:2015), "tbl_df")
  expect_equal(names(fars_summarize_years(2013:2015)), c("MONTH", 2013:2015))
  expect_error(fars_summarize_years(2020))
})

# test fars_map_state
test_that("test fars_map_state() run well", {
  expect_silent(fars_map_state(1, 2013))
  expect_error(fars_map_state(60, 2013))
  expect_error(fars_map_state(10, 2020))
})
