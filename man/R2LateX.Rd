\name{R2LateX}
\alias{R2LateX}
\title{Write an R file into a LateX file}
\usage{
R2LateX <- function(Rfile,
  FileName = deparse(substitute(Rfile)),
  Options = "12pt, a4paper",
  class = "article",
  packages = c("ctex", "[a4paper, tmargin=1in]{geometry}"),
  moreoptions = c("\\setCJKmainfont{Source Han Serif SC}")
)
}
\description{
Draw Bayes Factor Based on the Alpha Level.
}
\arguments{
  \item{FileName}{File to be converted}
  \item{Options}{Options passed to document options in LateX}
  \item{class}{Options passed to document class in LateX}
  \item{packages}{Options passed to document \\usepackage{} in LateX} 
  \item{moreoptions}{more options passed to the premable area of the Latex File} 
}
\examples{
example <- c("\\\\begin{enumerate}", "\\\\item The first element", "\\\\item The second element", "\\\\end{enumerate}")
R2LateX(example)
}
