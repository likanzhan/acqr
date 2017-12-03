\name{StudRes}
\alias{StudRes}
\title{Studentized Residuals}
\description{
Studentized Residuals
}
\arguments{
  \item{formula}{It can either be an lm object, or an formula}
  \item{data}{If formula is an lm object, this argument will be neglated}
}
\usage{
StudRes(formula, data)
}
\examples{
library(car)
Davis <- Davis[!is.na (Davis[, "repwt"]), ]
Davis[, "sex"] <- factor(
   Davis[, "sex"], levels = c("M", "F"))
   
fm01 <- lm(repwt ~ weight * sex, data = Davis)
rstudent(fm01)
StudRes(repwt ~ weight * sex, data = Davis)
StudRes(fm01)

fm02 <- lm(prestige ~ education + income, data = Duncan)
rstudent(fm02)
StudRes(prestige ~ education + income, data = Duncan)
StudRes(fm02)
}
