test_that("fileInput created from add_fileInput is shiny tag", {
  expect_type(add_fileInput(c("text/csv",'csv'), FALSE), "list")
  expect_true(class(add_fileInput( c("text/csv",'csv'), FALSE)) == "shiny.tag")
  expect_type(add_fileInput(c("text/csv",'csv'), TRUE), "list")
  expect_true(class(add_fileInput( c("text/csv",'csv'), TRUE)) == "shiny.tag")
})