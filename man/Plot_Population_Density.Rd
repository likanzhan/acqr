\name{Plot_Population_Density}
\alias{Plot_Population_Density}
\title{Plot_Population_Density}
\description{
Plot_Population_Density
}
\arguments{
  \item{mean}{mean}
  \item{sd}{standard deviation}
  \item{X}{the range of Xs}
  \item{AZ}{whether to supress thez axis or not}
  \item{AX}{whether to supress the X axis or not}
  \item{arrow}{whether to supress the arrow or not}
}
\usage{
Plot_Population_Density(mean, sd, point = NULL, X = seq((mean - 3 * sd),  (mean + 3 * sd), by = 0.01), AZ = FALSE, AX = FALSE, arrow = FALSE)
}
\examples{
Plot_Population_Density(3, 1)
}
