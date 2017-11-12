\name{help_console}
\alias{help_console}
\title{Printing R help files in the console or in knitr documents}
\usage{
help_console(optim, "html", lines = 1:25, before = " ", after = " ")
}
\description{
Printing R help files in the console or in knitr documents
}
\arguments{
  \item{topic}{Size of Type I error}
  \item{format}{"text", "html", "latex", "Rd"}
  \item{lines}{SNULL}
  \item{before}{NULL}
  \item{after}{NULL}
}
\examples{
help_console(optim, "html", lines = 1:25, before = " ", after = " ")
}

\references{
https://www.r-bloggers.com/printing-r-help-files-in-the-console-or-in-knitr-documents/
}
