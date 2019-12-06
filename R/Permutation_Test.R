#' Compute the permutation test of correlation
#' @param p,q two data sets
#' @param size size = 1000, size of the permutation
#' @examples
#' p <- c(5.0, 4.8, 4.7, 4.0, 5.3, 4.1, 5.5, 4.7, 3.3, 4.0, 4.0, 4.6, 5.3, 3.0, 3.5, 3.9, 4.7, 5.0, 5.2, 4.6)
#' q <- c(60, 59, 58, 47, 65, 48, 67, 70, 55, 63, 62, 65, 71, 56, 59, 60, 74, 77, 78, 62)
#' Permutation_Test(p, q)
#' @export

Permutation_Test <- function(
                             p, q, size = 1000) {
  original <- cor(p, q)
  ss <- function() sample(p, length(p), replace = FALSE)
  res <- replicate(size, cor(ss(), q))
  prob <- length(res[res >= original]) / size
  diagram <- hist(res, main = "")
  abline(v = original, col = "red")
  text(x = original, y = max(diagram$counts) / 4, label = bquote("p" == .(prob)), pos = 2, srt = 0, col = "red")
}
