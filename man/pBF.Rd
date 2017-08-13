\name{pBF}
\alias{pBF}
\title{Draw Bayes Factor Based on the Alpha Level}
\usage{
pBF(type1 = 0.05, type2 = 0.25, UMPTB = 0.005)
}
\description{
Draw Bayes Factor Based on the Alpha Level.
}
\arguments{
  \item{type1}{Size of Type I error}
  \item{type1}{Size of Type II error}
  \item{UMPTB}{Size of UMPTB}
}
\examples{
pBF()
pBF(type1 = 0.05, type2 = 0.25, UMPTB = 0.005)
pBF(type1 = 0.01, type2 = 0.20, UMPTB = 0.005)
}
