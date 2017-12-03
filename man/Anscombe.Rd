\name{Anscombe}
\alias{Anscombe}
\title{Anscombe Insurance Analogy}
\description{
Anscombe Insurance Analogy
}
\arguments{
  \item{model}{lm model}
  \item{q=0.05}{Premium level}
}
\usage{
	Anscombe(model, q = 0.05)
}
\examples{
library(car)
Davis <- Davis[!is.na (Davis[, "repwt"]), ]
Davis[, "sex"] <- factor(
   Davis[, "sex"], levels = c("M", "F"))
fm01 <- lm(repwt ~ weight * sex, data = Davis)
Anscombe(fm01)
}
