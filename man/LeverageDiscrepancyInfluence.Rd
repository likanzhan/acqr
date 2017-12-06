\name{LeverageDiscrepancyInfluence}
\alias{LeverageDiscrepancyInfluence}
\title{LeverageDiscrepancyInfluence}
\description{
Plot the relation between leverage, discrepancy, and influence
}
\arguments{
  \item{n=5}{Length of original observations}
  \item{slope = -1}{Slope of the original sample}
  \item{NP = c(n+1, 0, n+1, -1)}{New Observations added to the data set}
  \item{adj = 1}{Adjustment of the ranges of the two axes}
}
\usage{
JointInfl (n = 5, slope = -1, NP = c(n + 1, 0, n+1, -1), adj = 1)
}
\examples{
JointInfl()
}
