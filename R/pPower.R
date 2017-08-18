pPower <- function (
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