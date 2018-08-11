IntAcMain <- function(
                      R = 2, C = 3,
                      data = 1:(R * C),
                      adjust = 1) {
  RowLevel <- paste("R", 1:R, sep = "")
  ColLevel <- paste("C", 1:C, sep = "")
  ge <- expand.grid(
    C = ColLevel, R = RowLevel,
    KEEP.OUT.ATTRS = TRUE, stringsAsFactors = FALSE
  )
  # jitter <- 1:C
  # for (i in 1:R){ge[C * (i -1 ) + (1:C), "val"] <- 1:C + i * jitter}
  ge[, "val"] <- data
  pch <- c(1, 19, 21, 0, 15, 22, 5, 18, 23)[1:R]
  with(
    ge,
    interaction.plot(
      x.factor = C, trace.factor = R, response = val,
      type = "b", pch = pch,
      ylim = c(mean(data) - 5 * adjust, mean(data) + 5 * adjust),
      yaxt = "n", ylab = "", xlab = "", las = 1
    )
  )
  mtext("Y", side = 3, las = 1, adj = -0.03)
}
# IntAcMain()
