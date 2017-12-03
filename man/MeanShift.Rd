\name{MeanShift}
\alias{MeanShift}
\title{Mean Shift Outlier Model}
\description{
Mean Shift Outlier Model
}
\arguments{
  \item{formula}{It can either be an lm object, or an formula}
  \item{data}{If formula is an lm object, this argument will be neglated}
}
\usage{
MeanShift(formula, data)
}
\examples{
library(car)
Davis <- Davis[!is.na (Davis[, "repwt"]), ]
Davis[, "sex"] <- factor(
   Davis[, "sex"], levels = c("M", "F"))
   
fm01 <- lm(repwt ~ weight * sex, data = Davis)
rstudent(fm01)
MeanShift(repwt ~ weight * sex, data = Davis)
MeanShift(fm01)

fm02 <- lm(prestige ~ education + income, data = Duncan)
rstudent(fm02)
MeanShift(prestige ~ education + income, data = Duncan)
MeanShift(fm02)
}
