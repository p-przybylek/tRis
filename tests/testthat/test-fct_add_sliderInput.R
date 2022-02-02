test_that("sliderInput created from add_sliderInput is shiny tag for RRRR format", {
  vector_time <- c(2000, 2005, 2010)
  expect_type(add_sliderInput(vector_time, "one_value"), "list")
  expect_true(class(add_sliderInput(vector_time, "one_value")) == "shiny.tag")
  
  expect_type(add_sliderInput(vector_time, "period"), "list")
  expect_true(class(add_sliderInput(vector_time, "period")) == "shiny.tag")
})

test_that("sliderInput created from add_sliderInput is shiny tag for RRRR-MM-DD format", {
  vector_time <- c("2020-01-01","2020-01-02")
  expect_type(add_sliderInput(vector_time, "one_value"), "list")
  expect_true(class(add_sliderInput(vector_time, "one_value")) == "shiny.tag")
  
  expect_type(add_sliderInput(vector_time, "period"), "list")
  expect_true(class(add_sliderInput(vector_time, "period")) == "shiny.tag")
})