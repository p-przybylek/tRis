test_that("helper and shiny object created from add_helper is shiny tag list", {
  expect_type(add_helper(actionButton("return_to_start_button", "return"), "text"), "list")
  expect_true(all(class(add_helper(actionButton("return_to_start_button", "return"), "text")) == c("shiny.tag.list", "list")))
})