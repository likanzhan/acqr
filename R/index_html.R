index_html <- function(
  path = "~/Dropbox/Apps/updog/appendix"
){
  files <- list.files(path, full.names = TRUE)
  files <- files[basename(files) != "index.html"]
  pre_list <- paste(
    "<!DOCTYPE html>", "\n", 
    "<html>", "\n",
    "<head>", "\n",
    "<title> ", basename(path), " </title>", "\n",
    "</head>", "\n", 
    "<body>", "\n",
    "<ul>", "\n", sep = "")
  post_list <- paste(
    "</ul>", "\n", 
    "</body>", "\n", 
    "</html>", sep = "")
  item <- function(i) {
  	  i_dir <- ifelse(file_test("-d", i), paste(basename(i), "/index.html", sep = ""), i)
      paste("<li> ", "<a href=", "\"", i_dir, "\"", "> ", basename(i), " </li>", "\n", sep = "")
  }
  file_list <- sapply(files, item, USE.NAMES = FALSE)
  document <- c(pre_list, file_list, post_list)
  cat(document, file = paste(path, "/index.html", sep = ""))	
}
