#' LeverageDiscrepancyInfluence
#' @export

LeverageDiscrepancyInfluence <- function(
                                         Points = c(mean(1:5), 1),
                                         n = 5,
                                         slope = 1,
                                         adj = 1) {
  if (length(Points) >= 2) {
    df0 <- data.frame(X = 1:n, Y = slope * (1:n), GR = 1)
    df1 <- as.data.frame(matrix(Points, ncol = 2, byrow = TRUE))
    df1[, "GR"] <- 2:(nrow(df1) + 1)
    colnames(df1) <- c("X", "Y", "GR")
    df2 <- rbind(df0, df1)
  } else {
    df2 <- df0
  }

  plot(Y ~ X,
    data = df2, cex = 2.5,
    xlim = c(min(X) - adj, max(X) + adj),
    ylim = c(min(Y) - adj, max(Y) + adj),
    xaxt = "n", yaxt = "n", xlab = "", ylab = "", font.lab = 2,
    pch = rep(c(1, 10, 19, 5, 9, 23), ceiling(max(GR) / 6))[GR]
  )
  for (i in 1:max(df2[, "GR"])) {
    abline(
      lm(Y ~ X, data = df2[1:(n + i - 1), ]),
      lty = ifelse(i == 1 | i == max(df2[, "GR"]), 1, 1 + i),
      lwd = ifelse(i == max(df2[, "GR"]), 2, 1)
    )
  }
  mtext("Y", side = 2, at = max(df2[, "Y"]) + adj, las = 2, adj = 1.5)
  mtext("X", side = 1, at = max(df2[, "X"]) + adj, las = 1, padj = 0.2)
}
