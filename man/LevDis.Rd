\name{LevDis}
\alias{LevDis}
\title{Leverage, Discrepancy and Influences}
\usage{
LevDis <- function(
  n = 7, x0 = mean(1:n), y0 = -1, slope = 1, 
  title = "a). A low leverage outlier"
)
}
\description{
Plot the Relation between Leverage, Discrepancy and Influences of observations
}
\arguments{
  \item{n=7}{The number of original observations}
  \item{x0}{The X value of the new observation}
  \item{y0}{The y value of the new observation} 
  \item{slope = 1}{The slope of the oririnal data}
  \item{title}{The title of the plot} 
}
\examples{
LevDis()
}
