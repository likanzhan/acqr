#' Calculate Standard Error
StdErr <- function(
                   sample_size = 1000,
                   sample_number = 1000,
                   mean = 0,
                   sd = 10) {
  data <- rnorm(sample_size * sample_number)
  data <- scale(data)
  data <- mean + sd * data
  mdat <- matrix(data, nrow = sample_number)
  em <- rowMeans(mdat)
  ss <- sum(em^2) - (sum(em))^2 / sample_number
  se2 <- ss / (sample_number - 1)
  se <- sqrt(se2)
  return(se)
}

#' @export

Calculate_Standard_Error <- function(
                                     sample_size = 1000, sample_number = 1000,
                                     mean = 0, sd = 10,
                                     format = NA) {
  if (length(sample_size) == 1) {
    stde <- StdErr(sample_size, sample_number, mean, sd)
    res <- data.frame("Sample_Size" = sample_size, "Standard_Error" = stde)
  } else {
    stde <- sapply(sample_size, FUN = function(sample_size) StdErr(sample_size, sample_number, mean, sd))
    res <- data.frame("Sample_Size" = sample_size, "Standard_Error" = stde)
  }
  if (!is.na(format) && format == "xtable") {
    res <- xtable::xtable(res, digits = c(rep(0, length(res)), 2), align = rep("c", length(res) + 1))
  }
  return(res)
}
