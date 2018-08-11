R2LateX <- function(
                    TeXFile,
                    Options = "12pt, a4paper",
                    class = "article",
                    packages = c("ctex", "[a4paper, tmargin=1in]{geometry}"),
                    moreoptions = c("\\setCJKmainfont{Source Han Serif SC}")) {
  OPT <- as.numeric(gregexpr(pattern = "]", text = packages))
  PKGs <- ifelse(OPT > 0,
    paste("\\usepackage", substr(packages, 1, OPT),
      substr(packages, OPT + 1, nchar(packages)),
      sep = ""
    ),
    paste("\\usepackage", "{", packages, "}", sep = "")
  )
  LaTeX <- c(
    paste(
      "\\documentclass[", Options, "]{", class, "}",
      sep = ""
    ),
    PKGs, moreoptions, "\\begin{document}", TeXFile, "\\end{document}"
  )
  return(LaTeX)
}
