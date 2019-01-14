#' Best Programming Language
#' What is the Best Programming Language in History?
#' @import getPass getPass
#' @import digest  digest

#' @export

Language <- function(){ 
  pw <- getPass::getPass(msg = "The Best Programming Language is ?")
  Hash <- "71ec0b920622cf4358bbc21d6a8b41f903584808db53ec07a8aa79119304ce86"
  if (identical(digest::digest(pw, "sha256"), Hash)) "You Are Correct !" else "You are Incorrect !"
}