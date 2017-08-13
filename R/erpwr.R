pEC <- function (
  # m0 = 80, m1 = 88, sigma = 10, n = 25, sig = 0.05,
  m0 = 0, m1 = 4, sigma = 1, n = 1, sig = 0.05,
  H0 = TRUE, H1 = TRUE, body = TRUE, tail = TRUE, beta = TRUE, power = TRUE,
  reject = TRUE, level = TRUE, AxisZ = FALSE
)
{
  # Calculate critical values
  sigma_m <- sigma / sqrt(n)
  d <- (m1 - m0) / sigma
  # X and Ys
  X0s <- seq(m0 - 5 * sigma_m, m0 + 5 * sigma_m,by = 0.01 * sigma_m)
  X1s <- seq(m1 - 5 * sigma_m, m1 + 5 * sigma_m, by = 0.01 * sigma_m)
  Xs <- sort(cbind(X0s, X1s))
  Y0s <- dnorm(Xs, mean = m0, sd = sigma_m)
  Y1s <- dnorm(Xs, mean = m1, sd = sigma_m)
  # Critical X values
  X_min <- qnorm(sig / 2, mean = m0, sd = sigma_m, lower.tail = TRUE)
  X_max <- qnorm(sig / 2, mean = m0, sd = sigma_m, lower.tail = FALSE)
  # polygons
  col2alpha <- function(col, alpha) {
    col_rgb <- col2rgb(col)/255
    rgb(col_rgb[1], col_rgb[2], col_rgb[3], alpha = alpha)
  }
  ## Tail
  low_tail <- seq(min(Xs), X_min, by = sigma_m * 0.001)
  high_tail <- seq (X_max, max(Xs), by = sigma_m * 0.001)
  alpha_value <- sub("^0\\.", ".", formatC(sig/2, digits = 2, format = "fg"))
  alpha_com <- sub("^0\\.", ".", formatC(1 - sig, digits = 2, format = "fg"))
  beta_value <- pnorm(q = high_tail, mean = m1, sd = sigma_m)
  beta_value <- formatC(beta_value, digits = 2, format = "fg")
  beta_value <- sub("^0\\.", ".", beta_value)
  power_value <- pnorm(q = high_tail, mean = m1, sd = sigma_m, lower.tail = FALSE)
  power_value <- formatC(power_value, digits = 2, format = "f")
  power_value <- sub("^0\\.", ".", power_value)
  boddy <- seq(X_min, X_max, by = sigma_m * 0.001)
  tail_x <- c(min(low_tail), low_tail, max(low_tail), min(high_tail), high_tail, max(high_tail) )
  tail_y <- c(0, dnorm(low_tail, mean = m0, sd = sigma_m), 0, 0,
              dnorm(high_tail, mean = m0, sd = sigma_m), 0)
  tail_col <- col2alpha("#DC143C", alpha = 0.5)
  body_x <- c(min(boddy), boddy, max(boddy))
  body_y <- c(0, dnorm(boddy, mean = m0, sd = sigma_m), 0)
  body_col <- col2alpha("#D2691E", alpha = 0.5)
  beta_range <- seq(min(min(X1s), X_max), max(min(X1s), X_max), by = sigma_m * 0.001)
  beta_x <- c(min(X1s), beta_range, X_max)
  beta_y <- c(0, dnorm(beta_range, mean = m1, sd = sigma_m), 0)
  beta_col <- col2alpha("#00008B", alpha = 0.5)
  power_range <- seq( X_max, max(X1s), by = sigma_m * 0.001)
  power_x <- c(X_max, power_range, max(X1s))
  power_y <- c(0, dnorm(power_range, mean = m1, sd = sigma_m), 0)
  power_col <- col2alpha("#1E90FF", alpha = 0.5)
  #label_pos <- seq(60, 105, by = sigma_m)
  label_pos <- seq(min(Xs), max(Xs), by = sigma_m)
  ## plot
  plot(NULL, NULL, xlim = range(Xs), # c(60, 105)
       ylim = c(0, (1 + 0.01) * max(Y0s) ),
       axes = FALSE, xlab = "", ylab = "", yaxs = "i")
  # polygons
  if (tail) {
    polygon(tail_x, tail_y, col = tail_col, border = "white")
    if (level){
      text(x = X_min - sigma_m * 0.5, y = dnorm(m0, m0, sd = sigma_m) / 15,
           bquote(alpha / 2 == .(alpha_value)), pos = 2)
    }
  }
  if (body) {
    polygon( body_x, body_y, col = body_col, border = "white")
    if (level){
      text(x = m0 - sigma_m * 0.6, y = dnorm(m0, m0, sd = sigma_m) / 2,
           bquote(1 - alpha == .(alpha_com)), pos = 3)
    }
  }

  if (beta) {
    polygon(beta_x, beta_y, col = beta_col, border = "white")
    if (level){
      text(x = X_max - sigma_m * 0.4, y = dnorm(m1, m1, sd = sigma_m) / 15,
           labels = bquote(beta == .(beta_value)), pos = 2)
    }
  }
  if (power) {
    polygon(power_x, power_y, col = power_col, border = "white")
    if (level){
      text(x = X_max + abs(m1 - X_max) * 0.5,
           y = dnorm(m1, m1, sd = sigma_m) / 15,
           labels = bquote(1-beta == .(power_value)), pos = 4)
    }
  }
  # H_0
  if (H0){
    lines(Xs, Y0s, col = "#cf232a", lwd = 2)
    abline(v = m0, lwd = 1, col = "gray")
    mtext(expression(H[0]), side = 3, at = m0)
    arrows(x0 = m0, y0 = dnorm(m0 + sigma_m, m0, sigma_m),
           x1 = m0 + sigma_m, y1 = dnorm(m0 + sigma_m, m0, sigma_m),
           length = 0.1,  col = "gray")
    text(labels = bquote(sigma[M] == .(round(sigma_m, 2))), x = m0 + sigma_m / 2,
         y = dnorm(m0 + sigma_m, m0, sigma_m), pos = 3, col = "gray")
  }
  # H_1
  if (H1){
    lines(Xs, Y1s, col = "#d5493a", lwd = 2)
    abline(v = m1, lwd = 1, col = "gray")
    mtext(expression(H[1]),  side = 3, at = m1)
    mtext(text = bquote("Cohen's d" == .(d)~", n"==.(n)), side = 3, at = (m0 + m1) / 2,
          padj = -2)
  }
  ## X-Axes

  axis(1, at = label_pos, label = round(label_pos, 2))
  mtext(text = "X", side = 1, adj = 1, line = -0.6)
  if (AxisZ){
    axis(side = 1, at = label_pos, label = (label_pos - m0) / sigma_m , line = 2.5)
    mtext(text = "Z", side = 1, adj = 1, line = 1.9)
  }

  ## Critical lines
  if(reject){
    abline(v = X_max, lty = 3)
    text( x = X_max, y = dnorm(m0, m0, sd = sigma_m) / 1.1,
          label = expression(paste("Reject ", H[0])),  pos = 4)
    arrows(x0 = X_max, y0 = dnorm(m0, m0, sd = sigma_m) / 1.16,
           x1 = X_max + sigma_m, length = 0.1)
    abline(v = X_min, lty = 3)
    text(x = X_min, y = dnorm(m0, m0, sd = sigma_m) / 1.1,
         label = expression(paste("Reject ", H[0])), pos = 2)
    arrows(x0 = X_min, y0 = dnorm(m0, m0, sd = sigma_m) / 1.16,
           x1 = X_min - sigma_m, length = 0.1)
  }
}

############################
pBF <- function(
  type1 = 0.05, # alpha level is 0.05/2
  type2 = 0.25, # beta is 0.25, so that power is 1-0.25 = 0.75
  UMPTB = 0.005
){
  p = 1 - c(9000 : 9990) / 10000
  xbar = qnorm(p = 1 - p / 2, mean = 0, sd = 1)

  ## alternative based on 80% POWER IN 5% TEST
  muPower = qnorm(p = 1 - type2, mean = 0, sd = 1) +
    qnorm(p = 1 - type1 / 2, mean = 0, sd = 1)  # mu of the alternative with the power of 75%
  bfPow = 0.5 * (dnorm(x = xbar, mean =  muPower, sd = 1) +
                   dnorm(x = xbar, mean = -muPower, sd = 1)) /
    dnorm(x = xbar, mean = 0, sd = 1)
  muUMPBT = qnorm(p = 1 - UMPTB / 2, mean = 0, sd = 1)
  bfUMPBT = 0.5 * (dnorm(x = xbar, mean =  muUMPBT, sd = 1) +
                     dnorm(x = xbar, mean = -muUMPBT, sd = 1)) /
    dnorm(x = xbar, mean = 0, sd = 1)

  ## two-sided "LR" bound
  bfLR = 0.5 / exp(- 0.5 * xbar ^ 2)
  bfLocal = -1 / (exp(1) * p * log(p))  # Local bound; no need for two-sided adjustment
  yy <- cbind(bfLR, bfLocal)

  ## Coordinates for dashed lines
  data = data.frame(p, bfLocal, bfLR, bfPow, bfUMPBT)
  U_005 = max(data[data$p == "0.005", -1])
  L_005 = min(data[data$p == "0.005", -1])
  U_05  = max(data[data$p == "0.05", -1])
  L_05  = min(data[data$p == "0.05", -1])

  # plot margins
  # par(family = "Source Han Serif CN") # STKaiti
  par(mai = c(0.8, 0.8, 0.1, 0.6))
  par(mgp = c(2, 1, 0))
  matplot(p, yy, type = 'n', log = 'xy',
          xlab = expression(paste(italic(P) , "-value")),
          ylab = "Bayes Factor",
          ylim = c(0.3, 100), bty = "n", xaxt = "n", yaxt = "n")
  lines(p, bfPow, col = "red", lty = 2, lwd = 2)
  lines(p, bfLR, col = "black", lty = 1, lwd = 2)
  lines(p, bfUMPBT, col = "blue", lty = 4, lwd = 2)
  lines(p, bfLocal, col = "#56B4E9", lty = 1, lwd = 2)
  legend(x = 0.015, y = 100,
         legend = c(expression(paste("Power")), "Likelihood Ratio Bound", "UMPBT",
                    expression(paste("Local-", italic(H)[1]," Bound"))),
         lty = c(2, 1, 4, 1), lwd = 2, col = c("red", "black", "blue", "#56B4E9"), cex = 0.8)
  ## customizing axes
  # x axis
  axis(side = 1,
       at = c(-2, 0.001, 0.0025, 0.005, 0.010, 0.025, 0.050, 0.100, 0.14),
       labels = c("", "0.0010", "0.0025", "0.0050", "0.0100", "0.0250", "0.0500", "0.1000", ""),
       lwd = 1, tck = -0.01, padj = -1.1, cex.axis = 0.8)

  # y axis on the left - main
  axis(side = 2,
       at = c(-0.2, 0.3, 0.5, 1, 2, 5, 10, 20, 50, 100),
       labels = c("", "0.3", "0.5", "1.0", "2.0", "5.0", "10.0", "20.0", "50.0", "100.0"),
       lwd = 1, las = 1, tck = -0.01, hadj = 0.6, cex.axis = 0.8)

  #y axis on the left - secondary (red labels)
  axis(side = 2, at = c(L_005, U_005),
       labels = c(formatC(L_005, digits = 2, format = "f"), formatC(U_005, digits = 2, format = "f")),
       lwd = 1, las = 1, tck = -0.01,
       hadj = 0.6, cex.axis = 0.6, col.axis = "red")

  #y axis on the right - main
  axis(side = 4, at = c(-0.2, 0.3, 0.5, 1, 2, 5, 10, 20, 50, 100),
       labels = c("","0.3","0.5","1.0","2.0","5.0","10.0","20.0","50.0","100.0"),
       lwd = 1, las= 1, tck = -0.01, hadj = 0.4, cex.axis = 0.8)

  # y axis on the right - secondary (red labels)
  axis(side = 4, at = c(L_05, U_05),
       labels = c(formatC(L_05, digits = 2, format = "f"), formatC(U_05, digits = 2, format = "f")),
       lwd = 1, las = 1, tck = -0.01, hadj = 0.4, cex.axis = 0.6, col.axis = "red")

  ### dashed lines
  segments(x0 = 0.000011, y0 = U_005, x1 = 0.005, y1 = U_005, col = "gray40", lty = 2)
  segments(x0 = 0.000011, y0 = L_005, x1 = 0.005, y1 = L_005, col = "gray40", lty = 2)
  segments(x0 = 0.005, y0 = 0.00000001, x1 = 0.005, y1 = U_005, col = "gray40", lty = 2)
  segments(x0 = 0.05, y0 = U_05, x1 = 0.14, y1 = U_05, col = "gray40", lty = 2)
  segments(x0 = 0.05, y0 = L_05, x1 = 0.14, y1 = L_05, col = "gray40", lty = 2)
  segments(x0 = 0.05, y0 = 0.00000001, x1 = 0.05, y1 = U_05, col = "gray40", lty = 2)
}


############

ppp <- function(){
  #graph margins
  pow1 = c(5 : 999) / 1000 # power range for 0.005 tests
  pow2 = c(50 : 999) / 1000 # power range for 0.05 tests

  pow   <- c("pow1", "pow2", "pow2", "pow1",  "pow2",  "pow1")
  alpha <- c( 0.005,   0.05,   0.05,  0.005,    0.05,   0.005)
  pi0   <- c(   5/6,    5/6,  10/11,  10/11,   40/41,   40/41)
  lty   <- c(     1,      2,      2,      1,       2,       1)
  col   <- c("blue", "blue",  "red",  "red", "green", "green")
  df <- data.frame(pow, alpha, pi0, lty, col, stringsAsFactors=FALSE)

  ddata <- function(
    pow = "pow1",
    alpha = 0.005,
    pi0 = 5/6,
    id
  ){
    N = 10^6
    pow = eval(parse(text = pow))
    # yy = alpha * N * pi0 / (alpha * N * pi0 + pow[id] * (1 - pi0) * N)
    yy = alpha * pi0 /(alpha * pi0 + pow[id] * (1 - pi0))
    return(yy)
  }

  lline <- function(x
  ){
    pow = df[x, 1]
    alpha = df[x, 2]
    pi0 = df[x, 3]
    lty = df[x, 4]
    col = df[x, 5]
    xx = eval(parse(text = pow))
    yy = ddata(pow = pow, alpha = alpha, pi0 = pi0)
    lines(xx, yy, lty = lty, col = col, lwd = 2)
  }

  # png("Figure_2.png", width = 10, height = 8, units = "in", res = 600) # Print the plot
  #par(family = "Source Han Serif CN") # STKaiti
  par(mai = c(0.8, 0.8, 0.1, 0.1))
  par(mgp = c(2, 1, 0))
  plot(x = pow1, y = ddata(), type='n', ylim = c(0, 1), xlim = c(0, 1.5),
       xlab = 'Power',
       ylab = 'False positive rate',
       bty = "n", xaxt = "n", yaxt = "n")
  #grid lines
  segments(x0 = -0.058, y0 = (0:5) * 0.2, x1 = 1, lty = 1, col = "gray92")
  for (x in 1:6) lline(x)

  #customizing axes
  aat <- c(-0.5, 0, 0.2, 0.4, 0.6, 0.8, 1.0)
  llabels <- c("", "0.0", "0.2", "0.4", "0.6", "0.8", "1.0")
  llegend <- c("Prior odds = 1:40", "Prior odds = 1:10", "Prior odds = 1:5")
  #llegend <- c("先验概率 = 1:40", "先验概率 = 1:10", "先验概率 = 1:5")
  axis(side = 2, at = aat, labels = llabels,
       lwd = 1, las = 1, tck = -0.01, hadj = 0.4, cex.axis = .8)
  axis(side = 1, at = aat, labels = llabels,
       lwd = 1, las = 1, tck = -0.01, padj = -1.1, cex.axis = .8)
  legend(x = 1.05, y = 1, legend = llegend,
         pch = c(15, 15, 15), col = c("green", "red", "blue"), cex = 1)

  library(pBrackets)
  odd_1_5_1  <- ddata(pow = "pow1", alpha = 0.005, pi0 = 5/6, id = 995)
  odd_1_5_2  <- ddata(pow = "pow2", alpha = 0.05, pi0 = 5/6, id = 950)
  odd_1_10_2 <- ddata(pow = "pow2", alpha = 0.05, pi0 = 10/11, id = 950)
  odd_1_10_1 <- ddata(pow = "pow1", alpha = 0.005, pi0 = 10/11, id = 995)
  odd_1_40_2 <- ddata(pow = "pow2", alpha = 0.05, pi0 = 40/41, id = 950)
  odd_1_40_1 <- ddata(pow = "pow1", alpha = 0.005, pi0 = 40/41, id = 995)

  t1label <- expression(paste(italic(P), " < 0.05 threshold"))
  t2label <- expression(paste(italic(P), " < 0.005 threshold"))
  text(1.11, (odd_1_5_2 + odd_1_40_2)/2, label = t1label, cex = 0.9,adj=0)
  text(1.11, (odd_1_5_1 + odd_1_40_1)/2, label = t2label, cex = 0.9,adj=0)
  brackets(1.03, odd_1_40_1, 1.03, odd_1_5_1, h = NULL, ticks = 0.5, curvature = 0.7, type = 1, col = 1, lwd = 1, lty = 1, xpd = FALSE)
  brackets(1.03, odd_1_40_2, 1.03, odd_1_5_2, h = NULL, ticks = 0.5, curvature = 0.7, type = 1, col = 1, lwd = 1, lty = 1, xpd = FALSE)
  #dev.off()
}
