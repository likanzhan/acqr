\name{Density_Plot}
\alias{Density_Plot}
\title{Density_Plot}
\description{
modified density plot
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
	Density_Plot(mean, sd, point = NULL, X = seq((mean - 3 * sd),  (mean + 3 * sd), by = 0.01), AZ = FALSE, AX = FALSE, arrow = FALSE)
}
\examples{
Density_Plot(3, 1)
}
