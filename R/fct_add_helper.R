#' Attaches a helper to a UI object.
#' 
#' @param fun \code{function} UI object to have helper attached
#' @param content \code{character} A title of a helper
#' 
#' @importFrom shinyhelper helper
#' @return A shiny.tag.list - UI object with helper attached
#' 
add_helper <- function(fun, content){
  shinyhelper::helper(fun,
                      icon = "question",
                      colour = "#C9CBCE",
                      type = "markdown",
                      buttonLabel = "OK",
                      content = content)
}