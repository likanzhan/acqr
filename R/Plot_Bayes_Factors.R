#' Plot Bayes Factors
#' @export

############################
pBF <- function(
                type1 = 0.05, # alpha level is 0.05/2
                type2 = 0.25, # beta is 0.25, so that power is 1-0.25 = 0.75
                UMPTB = 0.005) {
  p <- 1 - c(9000:9990) / 10000
  xbar <- qnorm(p = 1 - p / 2, mean = 0, sd = 1)

  ## alternative based on 80% POWER IN 5% TEST
  muPower <- qnorm(p = 1 - type2, mean = 0, sd = 1) +
    qnorm(p = 1 - type1 / 2, mean = 0, sd = 1) # mu of the alternative with the power of 75%
  bfPow <- 0.5 * (dnorm(x = xbar, mean = muPower, sd = 1) +
    dnorm(x = xbar, mean = -muPower, sd = 1)) /
    dnorm(x = xbar, mean = 0, sd = 1)
  muUMPBT <- qnorm(p = 1 - UMPTB / 2, mean = 0, sd = 1)
  bfUMPBT <- 0.5 * (dnorm(x = xbar, mean = muUMPBT, sd = 1) +
    dnorm(x = xbar, mean = -muUMPBT, sd = 1)) /
    dnorm(x = xbar, mean = 0, sd = 1)

  ## two-sided "LR" bound
  bfLR <- 0.5 / exp(-0.5 * xbar^2)
  bfLocal <- -1 / (exp(1) * p * log(p)) # Local bound; no need for two-sided adjustment
  yy <- cbind(bfLR, bfLocal)

  ## Coordinates for dashed lines
  data <- data.frame(p, bfLocal, bfLR, bfPow, bfUMPBT)
  U_005 <- max(data[data$p == "0.005", -1])
  L_005 <- min(data[data$p == "0.005", -1])
  U_05 <- max(data[data$p == "0.05", -1])
  L_05 <- min(data[data$p == "0.05", -1])

  # plot margins
  # par(family = "Source Han Serif CN") # STKaiti
  par(mai = c(0.8, 0.8, 0.1, 0.6))
  par(mgp = c(2, 1, 0))
  matplot(p, yy,
    type = "n", log = "xy",
    xlab = expression(paste(italic(P), "-value")),
    ylab = "Bayes Factor",
    ylim = c(0.3, 100), bty = "n", xaxt = "n", yaxt = "n"
  )
  lines(p, bfPow, col = "red", lty = 2, lwd = 2)
  lines(p, bfLR, col = "black", lty = 1, lwd = 2)
  lines(p, bfUMPBT, col = "blue", lty = 4, lwd = 2)
  lines(p, bfLocal, col = "#56B4E9", lty = 1, lwd = 2)
  legend(
    x = 0.015, y = 100,
    legend = c(
      expression(paste("Power")), "Likelihood Ratio Bound", "UMPBT",
      expression(paste("Local-", italic(H)[1], " Bound"))
    ),
    lty = c(2, 1, 4, 1), lwd = 2, col = c("red", "black", "blue", "#56B4E9"), cex = 0.8
  )
  ## customizing axes
  # x axis
  axis(
    side = 1,
    at = c(-2, 0.001, 0.0025, 0.005, 0.010, 0.025, 0.050, 0.100, 0.14),
    labels = c("", "0.0010", "0.0025", "0.0050", "0.0100", "0.0250", "0.0500", "0.1000", ""),
    lwd = 1, tck = -0.01, padj = -1.1, cex.axis = 0.8
  )

  # y axis on the left - main
  axis(
    side = 2,
    at = c(-0.2, 0.3, 0.5, 1, 2, 5, 10, 20, 50, 100),
    labels = c("", "0.3", "0.5", "1.0", "2.0", "5.0", "10.0", "20.0", "50.0", "100.0"),
    lwd = 1, las = 1, tck = -0.01, hadj = 0.6, cex.axis = 0.8
  )

  # y axis on the left - secondary (red labels)
  axis(
    side = 2, at = c(L_005, U_005),
    labels = c(formatC(L_005, digits = 2, format = "f"), formatC(U_005, digits = 2, format = "f")),
    lwd = 1, las = 1, tck = -0.01,
    hadj = 0.6, cex.axis = 0.6, col.axis = "red"
  )

  # y axis on the right - main
  axis(
    side = 4, at = c(-0.2, 0.3, 0.5, 1, 2, 5, 10, 20, 50, 100),
    labels = c("", "0.3", "0.5", "1.0", "2.0", "5.0", "10.0", "20.0", "50.0", "100.0"),
    lwd = 1, las = 1, tck = -0.01, hadj = 0.4, cex.axis = 0.8
  )

  # y axis on the right - secondary (red labels)
  axis(
    side = 4, at = c(L_05, U_05),
    labels = c(formatC(L_05, digits = 2, format = "f"), formatC(U_05, digits = 2, format = "f")),
    lwd = 1, las = 1, tck = -0.01, hadj = 0.4, cex.axis = 0.6, col.axis = "red"
  )

  ### dashed lines
  segments(x0 = 0.000011, y0 = U_005, x1 = 0.005, y1 = U_005, col = "gray40", lty = 2)
  segments(x0 = 0.000011, y0 = L_005, x1 = 0.005, y1 = L_005, col = "gray40", lty = 2)
  segments(x0 = 0.005, y0 = 0.00000001, x1 = 0.005, y1 = U_005, col = "gray40", lty = 2)
  segments(x0 = 0.05, y0 = U_05, x1 = 0.14, y1 = U_05, col = "gray40", lty = 2)
  segments(x0 = 0.05, y0 = L_05, x1 = 0.14, y1 = L_05, col = "gray40", lty = 2)
  segments(x0 = 0.05, y0 = 0.00000001, x1 = 0.05, y1 = U_05, col = "gray40", lty = 2)
}
