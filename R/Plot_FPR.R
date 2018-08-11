############
pFPR <- function() {
  # graph margins
  pow1 <- c(5:999) / 1000 # power range for 0.005 tests
  pow2 <- c(50:999) / 1000 # power range for 0.05 tests

  pow <- c("pow1", "pow2", "pow2", "pow1", "pow2", "pow1")
  alpha <- c(0.005, 0.05, 0.05, 0.005, 0.05, 0.005)
  pi0 <- c(5 / 6, 5 / 6, 10 / 11, 10 / 11, 40 / 41, 40 / 41)
  lty <- c(1, 2, 2, 1, 2, 1)
  col <- c("blue", "blue", "red", "red", "green", "green")
  df <- data.frame(pow, alpha, pi0, lty, col, stringsAsFactors = FALSE)

  ddata <- function(
                      pow = "pow1",
                      alpha = 0.005,
                      pi0 = 5 / 6,
                      id) {
    N <- 10^6
    pow <- eval(parse(text = pow))
    # yy = alpha * N * pi0 / (alpha * N * pi0 + pow[id] * (1 - pi0) * N)
    yy <- alpha * pi0 / (alpha * pi0 + pow[id] * (1 - pi0))
    return(yy)
  }

  lline <- function(x) {
    pow <- df[x, 1]
    alpha <- df[x, 2]
    pi0 <- df[x, 3]
    lty <- df[x, 4]
    col <- df[x, 5]
    xx <- eval(parse(text = pow))
    yy <- ddata(pow = pow, alpha = alpha, pi0 = pi0)
    lines(xx, yy, lty = lty, col = col, lwd = 2)
  }

  # png("Figure_2.png", width = 10, height = 8, units = "in", res = 600) # Print the plot
  # par(family = "Source Han Serif CN") # STKaiti
  par(mai = c(0.8, 0.8, 0.1, 0.1))
  par(mgp = c(2, 1, 0))
  plot(
    x = pow1, y = ddata(), type = "n", ylim = c(0, 1), xlim = c(0, 1.5),
    xlab = "Power",
    ylab = "False positive rate",
    bty = "n", xaxt = "n", yaxt = "n"
  )
  # grid lines
  segments(x0 = -0.058, y0 = (0:5) * 0.2, x1 = 1, lty = 1, col = "gray92")
  for (x in 1:6) lline(x)

  # customizing axes
  aat <- c(-0.5, 0, 0.2, 0.4, 0.6, 0.8, 1.0)
  llabels <- c("", "0.0", "0.2", "0.4", "0.6", "0.8", "1.0")
  llegend <- c("Prior odds = 1:40", "Prior odds = 1:10", "Prior odds = 1:5")
  # llegend <- c("先验概率 = 1:40", "先验概率 = 1:10", "先验概率 = 1:5")
  axis(
    side = 2, at = aat, labels = llabels,
    lwd = 1, las = 1, tck = -0.01, hadj = 0.4, cex.axis = .8
  )
  axis(
    side = 1, at = aat, labels = llabels,
    lwd = 1, las = 1, tck = -0.01, padj = -1.1, cex.axis = .8
  )
  legend(
    x = 1.05, y = 1, legend = llegend,
    pch = c(15, 15, 15), col = c("green", "red", "blue"), cex = 1
  )

  library(pBrackets)
  odd_1_5_1 <- ddata(pow = "pow1", alpha = 0.005, pi0 = 5 / 6, id = 995)
  odd_1_5_2 <- ddata(pow = "pow2", alpha = 0.05, pi0 = 5 / 6, id = 950)
  odd_1_10_2 <- ddata(pow = "pow2", alpha = 0.05, pi0 = 10 / 11, id = 950)
  odd_1_10_1 <- ddata(pow = "pow1", alpha = 0.005, pi0 = 10 / 11, id = 995)
  odd_1_40_2 <- ddata(pow = "pow2", alpha = 0.05, pi0 = 40 / 41, id = 950)
  odd_1_40_1 <- ddata(pow = "pow1", alpha = 0.005, pi0 = 40 / 41, id = 995)

  t1label <- expression(paste(italic(P), " < 0.05 threshold"))
  t2label <- expression(paste(italic(P), " < 0.005 threshold"))
  text(1.11, (odd_1_5_2 + odd_1_40_2) / 2, label = t1label, cex = 0.9, adj = 0)
  text(1.11, (odd_1_5_1 + odd_1_40_1) / 2, label = t2label, cex = 0.9, adj = 0)
  brackets(1.03, odd_1_40_1, 1.03, odd_1_5_1, h = NULL, ticks = 0.5, curvature = 0.7, type = 1, col = 1, lwd = 1, lty = 1, xpd = FALSE)
  brackets(1.03, odd_1_40_2, 1.03, odd_1_5_2, h = NULL, ticks = 0.5, curvature = 0.7, type = 1, col = 1, lwd = 1, lty = 1, xpd = FALSE)
  # dev.off()
}
