#' Plot the theoretical quantile-quantile plot (q-q plot)
#'
#' @param family = c("c", "f", "g", "t", "n").
#' @param ... arguments passed to the cdf function.
#' @param adj = FALSE whether to adjust the quantile of the function.
#' @examples
#' Plot_Population_Quantile_Quantile("c", df = 3)
#' Plot_Population_Quantile_Quantile("t", df = 3)
#' Plot_Population_Quantile_Quantile("f", df1 = 5, df2 = 3, adj = TRUE)
#' @importFrom ggsci pal_npg
#' @export

Plot_Population_Quantile_Quantile <- function(
                                              family = c("c", "f", "g", "t", "n"), ..., adj = FALSE) {
  nrc <- ggsci::pal_npg("nrc", alpha = 1)(7)
  prpr <- seq(0, 1, length = 1000)
  family <- match.arg(family)
  qntlf <- switch(
    family,
    c = qchisq(prpr, ...),
    f = qf(prpr, ...),
    g = qgamma(prpr, ...),
    n = qnorm(prpr, ...),
    t = qt(prpr, ...)
  )
  qntly <- switch(
    family,
    c = paste0("Quantile of Chi-Square Distribution (", paste0(names(list(...)), " = ", list(...), collapse = ", "), ")"),
    f = paste0("Quantile of F Distribution (", paste0(names(list(...)), " = ", list(...), collapse = ", "), ")"),
    g = paste0("Quantile of Gamma Distribution (", paste0(names(list(...)), " = ", list(...), collapse = ", "), ")"),
    n = paste0("Quantile of Normal Distribution (", paste0(names(list(...)), " = ", list(...), collapse = ", "), ")"),
    t = paste0("Quantile of Student t Distribution (", paste0(names(list(...)), " = ", list(...), collapse = ", "), ")")
  )
  qntln <- qnorm(prpr, mean = 0, sd = 1)
  dffmin <- min(abs(qntlf - qntln)[complete.cases(abs(qntlf - qntln))])
  Adj <- ifelse(adj, dffmin, 0)
  plot(NULL,
    xlim = c(-3, 3), ylim = c(-3, 3) + Adj,
    xlab = "Quantile of Unit Normal Distribution (mean = 0, sd = 1)",
    ylab = qntly, xaxs = "i", yaxs = "i", font.lab = 1, col = "gray", lwd = 2
  )
  lines(qntln, qntlf, col = nrc[1])
  abline(a = Adj, b = 1, lty = "dashed")
}
