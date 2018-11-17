#' Create a Latin Squared Matrix
#' @export

Create_Latin_Square_Matrix <- function(n = 5, Latin = TRUE) {
  mtrx <- matrix(1:n, n, n, byrow = TRUE)
  if (Latin) {
    mtrx[1, c(1, 1:floor(n / 2) * 2)] <- 1:(floor(n / 2) + 1)
    mtrx[1, (2:ceiling(n / 2)) * 2 - 1 ] <- n + 2 - (2:ceiling(n / 2))
  }
  for (i in 2:n) mtrx[i, ] <- ifelse(mtrx[i - 1, ] + 1 <= n, mtrx[i - 1, ] + 1, mtrx[i - 1, ] + 1 - n)
  if((n %% 2) != 0) {
  	mtrx <- rbind(mtrx, t(apply(mtrx, 1, rev)))
  }
  colnames(mtrx) <- if (n <= 26) {
    LETTERS[1:n]
  } else {
    sprintf(paste("C%0", max(nchar(n)), "d", sep = ""), 1:n)
  }
  return(mtrx)
}
