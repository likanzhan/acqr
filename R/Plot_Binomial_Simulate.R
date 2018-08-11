Plot_Binomial_Simulate <- function(
   samples = 10000, 
   size = 20, 
   pi = 0.5
   ){
  values <- sample(c(0, 1), samples * size, replace = TRUE, prob = c(1 - pi, pi))
  value.mat <- matrix(values, ncol = size)
  Successes <- apply(value.mat, 1, sum)
  a1 <- round((table(Successes) / samples), 3)
  b1 <- round(dbinom(0:n, size, pi), 3)
  names(b1) <- 0:size
  breaks <-  c((-0.5 + 0):(size + 0.5))
  hist(Successes, breaks = breaks, freq = FALSE, axes = FALSE,
    xlim = c(-0.5, size + 0.5), ylim = c(0, max(a1, b1)), xlab = "", ylab = "", col = "#faebd7",
    main = "Theoretical Values Superimposed Over Histogram of Simulated Values")
  axis(1, at = c(-2, size + 1), labels = c("", ""), lwd.ticks = 0, line = 0)
  axis(1, at = 0:size, label = rep("", size + 1), lwd = 0, lwd.ticks = 1, line = 0)
  axis(1, at = 0:size, label = 0:size, lwd = 0, lwd.ticks = 0, line = 0 - 0.5)
  mtext(text = "Successes", side = 1, adj = 0.5, line = 1.6)
  axis(2, at = c(-0.5, max(a1, b1)), labels = c("", ""), lwd.ticks = 0, line = 0)
  axis(2, lwd.ticks = 1, line = 0)
  x <- 0:size
  fx <- dbinom(x, size, pi)
  lines(x, fx, type = "h", lty = "dashed", col = "#ff033e")
  lines(x, fx, type = "p", pch = 16, col = "#ff033e")
  # list(simulated.distribution = a1, theoretical.distribution = b1)
}
# Plot_Binomial_Simulate(pi = 0.3, size = 5)