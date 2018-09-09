index_html <- function(
  folder = "Provided_by_Author",
  path = "/Users/lzhan/Dropbox/Apps/updog/appendix/"
){
  directory <- paste(path, folder, sep = "")
  output_file <- paste(directory, "/index.html", sep = "")
  files <- list.files(path = directory)
  files <- files[files != "index.html"]
  pre_list <- paste(
    "<!DOCTYPE html>", "\n", 
    "<html>", "\n",
    "<head>", "\n",
    "<title> ", folder, " </title>", "\n",
    "</head>", "\n", 
    "<body>", "\n",
    "<ul>", "\n", sep = "")
  post_list <- paste(
    "</ul>", "\n", 
    "</body>", "\n", 
    "</html>", sep = "")
  item <- function(i) paste(
    "<li> ", "<a href=", "\"", i, "\"", "> ", i, " </li>", "\n", sep = "")
  file_list <- sapply(files, item, USE.NAMES = FALSE)
  document <- c(pre_list, file_list, post_list)
  cat(document, file = output_file)	
}
