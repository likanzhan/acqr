#' Create an Index HTML file
#' Create a HTML file listing the files in the current directory
#' @export

Create_Index_Html <- function(
                              path = "~/Dropbox/Apps/updog/appendix") {
  files_all <- list.files(path, full.names = TRUE)
  files_all <- files_all[basename(files_all) != "index.html"]
  folder_list <- files_all[file_test("-d", files_all)] # directory
  file_list <- files_all[file_test("-f", files_all)] # files
  folder_item <- function(i) paste("<li> ", "<a href=", "\"", basename(i), "/index.html", "\"", "> ", basename(i), " </li>", "\n", sep = "")
  file_item <- function(i) paste("<li> ", "<a href=", "\"", basename(i), "\"", "> ", basename(i), " </li>", "\n", sep = "")
  folder_list <- sapply(folder_list, folder_item, USE.NAMES = FALSE)
  file_list <- sapply(file_list, file_item, USE.NAMES = FALSE)
  if (length(folder_list) == 0) {
    folder_section <- ""
  } else {
    folder_section <- c("<li> Folders </li>", "\n", "<ul>", "\n", folder_list, "</ul>", "\n")
  }
  if (length(file_list) == 0) {
    file_section <- ""
  } else {
    file_section <- c("<li> Files </li>", "\n", "<ul>", "\n", file_list, "</ul>", "\n")
  }
  document_pre <- paste(
    "<!DOCTYPE html>", "\n",
    "<html>", "\n",
    "<head>", "\n",
    "<title> ", basename(path), " </title>", "\n",
    "<meta charset=", "\"", "utf-8", "\"", ">", "\n",
    "</head>", "\n",
    "<body>", "\n",
    "<ol>",
    sep = ""
  )
  document_post <- paste("</ol>", "</body>", "\n", "</html>", sep = "")
  document_full <- c(document_pre, folder_section, file_section, document_post)
  cat(document_full, file = paste(path, "/index.html", sep = ""))
}
