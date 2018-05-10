StdErr <- function(
  sample_size = 1000,
  sample_number = 1000,
  mean = 0,
  sd = 10
) {
  data <- rnorm(sample_size * sample_number, mean = mean, sd = sd)
  mdat <- matrix(data, nrow = sample_number)
  em <- rowMeans(mdat)
  ss <- sum(em ^ 2) - (sum(em)) ^ 2 / sample_number
  se2 <- ss / (sample_number - 1)
  se <- sqrt(se2)
  return(se)
}

Standard_Error <- function(
  sample_size = 1000,
  sample_number = 1000,
  mean = 0,
  sd = 10
){
  if (length(sample_size) == 1) {
    stde <- StdErr(sample_size, sample_number, mean, sd)
    data.frame("Sample_Size" = sample_size, "Standard_Error" = stde)
  } else{
    stde <- sapply(sample_size, FUN = function(sample_size) StdErr(sample_size, sample_number, mean, sd))
    data.frame("Sample_Size" = sample_size, "Standard_Error" = stde)
  }
}
