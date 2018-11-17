#' Plot the density of a Poisson distribution
#' @export

Plot_Poisson_Density <- function (
xx = 0:20,
#lambda = 4
lambda = c(4, 7, 10)
){
plot_pdf <- function(lambda) {
  yy <- dpois(xx, lambda)
  xmax <- max(xx)
  ymax <- max(yy)
  plot(xx, yy, xlim = c(0, xmax + 0.3), 
    type = "h", axes = FALSE,  xlab = "", ylab = "")
  box(col = "gray")
  axis(2, lwd = 0, lwd.ticks = 1, las = 1)
  points(xx, yy, pch = 16, col = "#6EA7CB", cex = 1.5)
  rect(xmax + 0.3, -1, xmax + 0.7, ymax + 1, col = "gray", border = "gray")
  text(xmax + 0.5, ymax / 2, srt = -90, label = bquote(labmda == .(lambda)))
  }
plot_cdf <- function(lambda) {
  xs <- rep(xx, round(dpois(xx, lambda) * 100000, 0))
  yy <- ecdf(xs)
  xmax <- max(xx)
  ymax <- max(yy(xx))
  plot(yy, xlim = c(0, xmax + 0.3), ylab = "", axes = FALSE, col.points = "#6EA7CB", cex.points = 1.5, main = "")
  box(col = "gray")
  axis(2, lwd = 0, lwd.ticks = 1, las = 1)
  rect(xmax + 0.3, -1, xmax + 0.7, ymax + 1, col = "gray", border = "gray")
  text(xmax + 0.5, ymax / 2, srt = -90, label = bquote(lambda == .(lambda)))
  }
par(
  mfcol = c(length(lambda), 2), 
  mai = c(0.1, 0.6, 0, 0),
  oma = c(3, 0, 4, 0.2),
  pty = "m")
for (i in lambda)	plot_pdf(lambda = i)
axis(1, lwd = 0, lwd.ticks = 1)
mtext(text = bquote(italic("x")), side = 1, line = 1.5, las = 1, outer = TRUE)
mtext(text = bquote(bold(atop("PDF (probability density function)", italic(P(X==x[i]))))),
  side = 3, at = 0.25, las = 1, outer = TRUE)

for (i in lambda) plot_cdf(lambda = i)
axis(1, lwd = 0, lwd.ticks = 1)
#mtext(text = expression("P(X"<="x)"), side = 4, outer = TRUE, at = 0.5, line = -18)
mtext(text = bquote(bold(atop("CDF (cumulative distribution function)", italic(P(X<=x[i]))))),
  side = 3, at = 0.75, las = 1, outer = TRUE)
}
#Plot_Poisson_Density()