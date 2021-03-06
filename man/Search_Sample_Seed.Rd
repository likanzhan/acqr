% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Search_Sample_Seed.R
\name{Search_Sample_Seed}
\alias{Search_Sample_Seed}
\title{Define a seed seach function
This function is used to find the appropriate seed to find the sample that does not contain
the elements with the same positions.}
\usage{
Search_Sample_Seed(vector, Seed_Initial = 1)
}
\description{
Define a seed seach function
This function is used to find the appropriate seed to find the sample that does not contain
the elements with the same positions.
}
\examples{
set.seed(Search_Sample_Seed(1:5))
sample(1:5)
}
