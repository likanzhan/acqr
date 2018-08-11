.Grid <- function(n) {
  ln <- list(0:1)
  rn <- rep(ln, n)
  eg <- expand.grid(rn)
  colnames(eg) <- paste("Toss", 1:n, sep = "-")
  rs <- rowSums(eg)
  egg <- eg
  egg[egg == 0] <- "Tails"
  egg[egg == 1] <- "Heads"
  egg[, "Number_of_Heads"] <- rs
  egg[, "Probability"] <- 1 / nrow(eg)
  res <- list(egg, rs)
  return(res)
}

Binomial_Density <- function(
                             n,
                             frequency = TRUE,
                             color = "#f8cea2",
                             borderc = "#b78135") {
  freq <- .Grid(n)[[2]]
  min <- min(freq) - 0.5
  max <- max(freq) + 0.5
  x <- seq(-0.5, n + 0.5, by = 0.01)
  y <- dnorm(x, n * 0.5, sqrt(n * 0.5 * (1 - 0.5)))
  hist(freq,
    breaks = seq(min, max, by = 1),
    ylim = c(0, ifelse(frequency, max(table(freq)), max(y))),
    yaxs = "i", col = color, border = borderc,
    axes = FALSE, freq = frequency,
    xlab = "Number of Heads",
    ylab = ifelse(frequency, "Counts", "Probability"),
    main = "", font.lab = 2
  )
  abline(h = 0)
  axis(1, at = seq(min + 0.5, max - 0.5), labels = seq(min + 0.5, max - 0.5))
  axis(2)
  abline(h = 0, lwd = 2)
  if (!frequency) lines(x, y, col = "#cf232a", lwd = 2)
}
