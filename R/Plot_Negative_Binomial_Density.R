#' Plot the density of a negative binomial distribution
#' @export

Plot_Negative_Binomial_Density <- function (
r = c(1, 3, 5),
pi = c(0.4, 0.6),
plot_density_function = TRUE,
xx = 0:20
){
ymaxd <- max(sapply(r, FUN = function(rr) sapply(pi, FUN = function(pipi) dnbinom(xx, rr, pipi))))
plot_pdf <- function(rr, pipi, 
  show_x_lab = FALSE,  show_y_lab = FALSE, 
  show_x_panel_lab = FALSE, show_y_panel_lab = FALSE
  ) {
  yy <- dnbinom(xx, rr, pipi)
  xmax <- max(xx)
  ymax <- ymaxd
  plot(xx, yy, xlim = c(0, xmax + 0.5), ylim = c(0, ymax * 1.1),
    type = "h", axes = FALSE,  xlab = "", ylab = "")
  axis(2, lwd = 1, at = c(-0.1, ymax * 1.1), labels = c("", ""), lwd.ticks = 0, col = "gray")
  if (show_y_lab) axis(2, lwd = 0, lwd.ticks = 1, las = 1)
  axis(1, lwd = 1, at = c(-1, xmax + 0.3), labels = c("", ""), lwd.ticks = 0, col = "gray")
  if (show_x_lab) axis(1, lwd = 0, lwd.ticks = 1, las = 1)
  points(xx, yy, pch = 16, col = "#6EA7CB", cex = 1.5)
  if (show_y_panel_lab) {
    rect(xmax + 0.3, -1, xmax + 0.9, ymax * 1.02, col = "gray", border = "gray")
    text(xmax + 0.6, ymax / 2, srt = -90, label = bquote(pi == .(pipi)))	
  }
  if (show_x_panel_lab){
    rect(-0.5, ymax * 1.02, xmax + 0.3, ymax * 1.1, col = "gray", border = "gray")
    text(xmax / 2, ymax * 1.06, srt = 0, label = bquote(r == .(rr)))	
  }
  }
plot_cdf <- function(rr, pipi, 
  show_x_lab = FALSE,  show_y_lab = FALSE, 
  show_x_panel_lab = FALSE, show_y_panel_lab = FALSE
  ) {
  xs <- rep(xx, round(dnbinom(xx, rr, pipi) * 100000, 0))
  yy <- ecdf(xs)
  xmax <- max(xx)
  ymax <- max(yy(xx))
  plot(yy, xlim = c(0, xmax + 0.3), ylim = c(0, ymax * 1.05), 
    xlab = "", ylab = "", axes = FALSE, 
    col.points = "#6EA7CB", cex.points = 1.5, main = "")
  axis(2, lwd = 1, at = c(-0.1, ymax * 1.1), labels = c("", ""), 
    lwd.ticks = 0, col = "gray")
  if (show_y_lab) axis(2, lwd = 0, lwd.ticks = 1, las = 1)
  axis(1, lwd = 1, at = c(-1, xmax + 0.3), 
    labels = c("", ""), lwd.ticks = 0, col = "gray")
  if (show_x_lab) axis(1, lwd = 0, lwd.ticks = 1, las = 1)
  if (show_y_panel_lab) {
  rect(xmax + 0.3, -1, xmax + 0.7, ymax * 1.02, col = "gray", border = "gray")
  text(xmax + 0.5, ymax / 2, srt = -90, label = bquote(pi == .(pipi)))
  }
    if (show_x_panel_lab){
  rect(-0.5, ymax * 1.02, xmax + 0.3, ymax * 1.1, col = "gray", border = "gray")
  text(xmax / 2, ymax * 1.06, srt = 0, label = bquote(r == .(rr)))
  }
  }
par(
  mfcol = c(length(pi), length(r)), 
  mai = c(0, 0, 0, 0),
  oma = c(4, 4.5, 0.5, 0.5),
  pty = "m")
for (rr in r) for (pipi in pi) if (plot_density_function) {
    plot_pdf(rr, pipi, 
    show_y_lab = (rr == min(r)), show_x_lab = (pipi == max(pi)),
	show_y_panel_lab = (rr == max(r)),show_x_panel_lab = (pipi == min(pi))
	)
    mtext(text = bquote(italic("x")), side = 1, line = 2.5, las = 1, outer = TRUE)
    mtext(text = bquote(bold("PDF (probability density function)"~italic(P(X==x[i])))),
          side = 2, at = 0.5, las = 3, line = 2.5, outer = TRUE)
	} else {
	    plot_cdf(rr, pipi, 
	    show_y_lab = (rr == min(r)), show_x_lab = (pipi == max(pi)),
	    show_y_panel_lab = (rr == max(r)),show_x_panel_lab = (pipi == min(pi))
	    )
	    mtext(text = bquote(italic("x")), side = 1, line = 2.5, las = 1, outer = TRUE)
        mtext(text = bquote(bold("PDF (probability density function)"~italic(P(X<=x[i])))),
            side = 2, at = 0.5, las = 3, line = 2.5, outer = TRUE)
          }
}
#Plot_Negative_Binomial_Density()