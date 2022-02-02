test_that("validate time column for RRRR works", {
  expect_false(time_column_check(c("2019", "2020", "2021")))
  expect_true(time_column_check(c("230", "231", "232")))
  expect_true(time_column_check(c("201", "2020", "2021")))
  expect_true(time_column_check(c("AAAA", "2020", "2021")))
})

test_that("validate time column for RRRR-MM-DD works", {
  expect_false(time_column_check(c("2019-09-09", "2020-05-01", "2021-11-06")))
  expect_true(time_column_check(c("2019-09-9", "2020-05-1")))
  expect_true(time_column_check(c("2019-09-9", "2020-05-01")))
  expect_true(time_column_check(c("2019-09-09", "AAAA-AA-AA")))
})

test_that("validate time column for RRRR.MM.DD works", {
  expect_false(time_column_check(c("2019.09.09", "2020.05.01", "2021.11.06")))
  expect_true(time_column_check(c("2019.09.9", "2020-05.1")))
  expect_true(time_column_check(c("2019.09.9", "2020.05.01")))
  expect_true(time_column_check(c("2019.09.09", "AAAA.AA.AA")))
  expect_true(time_column_check(c("2019.09.09", "AAAAAAAAAA")))
})

test_that("validate geo column for ISO aplha-1 and aplha-2 codes works", {
  expect_false(geo_column_check(c("AF", "PL"), "World"))
  expect_false(geo_column_check(c("AFG", "POL"), "World"))
  expect_true(geo_column_check(c("004", "616"), "World"))
  expect_true(geo_column_check(c("004", "6"), "World"))
  expect_true(geo_column_check(c("004", "6000"), "World"))
  expect_true(geo_column_check(c("004", "AAA"), "World"))
})

test_that("validate geo column for TERYT codes works", {
  expect_false(geo_column_check(c("32", "02"), "Poland"))
  expect_false(geo_column_check(c("t32", "t02"), "Poland"))
  expect_true(geo_column_check(c("AAAA", "02"), "Poland"))
  expect_true(geo_column_check(c("000000", "0000000000000"), "Poland"))
})