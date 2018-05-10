\name{Sample_Frequency_Plot}
\alias{Sample_Frequency_Plot}
\title{Sample_Frequency_Plot}
\description{
Modified_Frequency_Distribution_Mean
}
\arguments{
  \item{data}{the data to be drawn}
  \item{adjx}{adjustment of x}
  \item{mean_sd}{mean_sd=FALSE(default)}
  \item{AZ}{AZ=FALSE(default), if TRUE to show the z axis}
  \item{fill}{filled color of each cell}
  \item{border}{border color of each cell}
  \item{...}{arguments passed to hist}
}
\usage{
Modified_Frequency_Distribution(data, adjx = 1, mean_sd = FALSEm AZ = FALSE, fill = NULL, border = NULL, ...)
}
\examples{
X <- c(1, 1, 2, 3, 3, 4, 4, 4, 5, 6)
Modified_Frequency_Distribution(X)
}
