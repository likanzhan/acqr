#' Create the Empirical Cumulative Distribution Function (ECDF)
#'
#' @param data a numeric vector
#' @examples
#' set.seed(1)
#' nrm100 <- rnorm(100, mean = 50, sd = 10)
#' ECDF(nrm100)

ECDF <- function (data) {
       n <- length(data)
       mu <- median(data)
       sigma <- IQR(data) / 1.349 ## IQR: computes interquartile range
       FUN <- function (x) {
       	 Xi <- data[order(data)][x]
       	 i <- length(data[data <= Xi])
       	 Pi <- (i - 1 / 2) / n
       	 Zi <- qnorm(Pi, 0, 1)    ## Quantile Function-inverse of pnorm
       	 Pzi <- dnorm(Zi, 0, 1)   ## Probability Density Function (PDF)
       	 XiH <- mu + sigma * Zi
       	 SEi <- (sigma / Pzi) * sqrt((Pi * (1 - Pi)) / n)
       	 Env1 <- XiH - 2 * SEi 
       	 Env2 <- XiH + 2 * SEi
       	 c(Zi = Zi, Xi = Xi, XiH = XiH, Env1 = Env1, Env2 = Env2)
         }
       list <- lapply(1:n, fun)
       list <- do.call(rbind, list)
       list <- as.data.frame(list) 
       return(list)
      }

#' Plot the Empirical Quantile Quantile Diagram (q-q plot)
#'
#' @param data Empirical Data returened by `ECDF`
#' @examples
#' set.seed(1)
#' norm100 <- rnorm(100, mean = 50, sd = 10)
#' qql(nrm100)
#' Plot_Quantile_Quantile_Sample(nrm100)
#' @export

Plot_Quantile_Quantile_Sample <- function(data) {
DECDF <- ECDF(data)
Zi <- ecdf["Zi", ]; Xi <- ecdf["Xi", ]; XiH <- ecdf["XiH", ]
Env1 <- ecdf["Env1", ]; Env2 <- ecdf["Env2", ]
ylab <- expression(bold(paste("Sample from"~ N(50, 10^2))))
plot(NULL, xlim = c(-3, 3), ylim = range(DECDF[, "Xi"]), 
    font.lab = 2, xlab = "Normal Quantiles", ylab = ylab)
points(DECDF[, "Zi"], DECDF[, "Xi"])
lines (DECDF[, "Zi"], DECDF[, "XiH"], col = "red")
lines (DECDF[, "Zi"], DECDF[, "Env1"], lty = 2, lwd = 2, col = "blue")
lines (DECDF[, "Zi"], DECDF[, "Env2"], lty = 2, lwd = 2, col = "blue")
}
