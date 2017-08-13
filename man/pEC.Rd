\name{pEC}
\alias{pEC}
\title{Draw Type I error, Type II error, Power, Cohen's D}
\description{
Draw Type I error, Type II error, Power, Cohen's D
}
\arguments{
  \item{m0}{Population mean of the null hypothesis}
  \item{m1}{Population mean of the alternative hypothesis}
  \item{sigma}{Standard deviation of the two populations}
  \item{n}{Sample size}
  \item{sig}{Significant level}
  \item{H0}{Draw the density of H0}
  \item{H1}{Draw the density of H1}
  \item{body}{Add color to the body part of H0}
  \item{tail}{Add color to the tail part of H0}
  \item{beta}{Add color of the Type II error part of H1}
  \item{power}{Add color to the Power part of H1}
  \item{reject}{Show the annotations of "Reject H0"}
  \item{level}{Show the significant level}
  \item{AxisZ}{Show the x-axis based on the normalized value}
}
\usage{
pEC(m0 = 0, m1 = 4, sigma = 1, n = 1, sig = 0.05,
       H0 = TRUE, H1 = TRUE, body = TRUE, tail = TRUE, beta = TRUE, power = TRUE,
       reject = TRUE, level = TRUE, AxisZ = FALSE)
}
\examples{
pEC()
}
