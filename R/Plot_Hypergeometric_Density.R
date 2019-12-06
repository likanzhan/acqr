#' Plot the density of a hypergeometric distribution
#' @export

Plot_Hypergeometric_Density <- function(
                                        m = c(10, 40, 60),
                                        n = 10,
                                        k = c(5, 20),
                                        xx = 0:20,
                                        plot_density_function = TRUE) {
  ymaxd <- max(sapply(m, FUN = function(mm) sapply(k, FUN = function(kk) dhyper(xx, mm, n, kk))))
  plot_pdf <- function(mm, kk,
                       show_x_lab = FALSE, show_y_lab = FALSE,
                       show_x_panel_lab = FALSE, show_y_panel_lab = FALSE) {
    yy <- dhyper(xx, mm, n, kk)
    xmax <- max(xx)
    ymax <- ymaxd
    plot(xx, yy,
      xlim = c(0, xmax + 0.5), ylim = c(0, ymax * 1.1),
      type = "h", axes = FALSE, xlab = "", ylab = ""
    )
    axis(2, lwd = 1, at = c(-0.1, ymax * 1.1), labels = c("", ""), lwd.ticks = 0, col = "gray")
    if (show_y_lab) axis(2, lwd = 0, lwd.ticks = 1, las = 1)
    axis(1, lwd = 1, at = c(-1, xmax + 0.3), labels = c("", ""), lwd.ticks = 0, col = "gray")
    if (show_x_lab) axis(1, lwd = 0, lwd.ticks = 1, las = 1)
    points(xx, yy, pch = 16, col = "#6EA7CB", cex = 1.5)
    if (show_y_panel_lab) {
      rect(xmax + 0.3, -1, xmax + 0.9, ymax * 1.02, col = "gray", border = "gray")
      text(xmax + 0.6, ymax / 2, srt = -90, label = bquote(k == .(kk)))
    }
    if (show_x_panel_lab) {
      rect(-0.5, ymax * 1.02, xmax + 0.3, ymax * 1.1, col = "gray", border = "gray")
      text(xmax / 2, ymax * 1.06, srt = 0, label = bquote(m == .(mm)))
    }
  }
  plot_cdf <- function(mm, kk,
                       show_x_lab = FALSE, show_y_lab = FALSE,
                       show_x_panel_lab = FALSE, show_y_panel_lab = FALSE) {
    xs <- rep(xx, round(dhyper(xx, mm, n, kk) * 100000, 0))
    yy <- ecdf(xs)
    xmax <- max(xx)
    ymax <- max(yy(xx))
    plot(yy,
      xlim = c(0, xmax + 0.3), ylim = c(0, ymax * 1.05),
      xlab = "", ylab = "", axes = FALSE,
      col.points = "#6EA7CB", cex.points = 1.5, main = ""
    )
    axis(2,
      lwd = 1, at = c(-0.1, ymax * 1.1), labels = c("", ""),
      lwd.ticks = 0, col = "gray"
    )
    if (show_y_lab) axis(2, lwd = 0, lwd.ticks = 1, las = 1)
    axis(1,
      lwd = 1, at = c(-1, xmax + 0.3),
      labels = c("", ""), lwd.ticks = 0, col = "gray"
    )
    if (show_x_lab) axis(1, lwd = 0, lwd.ticks = 1, las = 1)
    if (show_y_panel_lab) {
      rect(xmax + 0.3, -1, xmax + 0.7, ymax * 1.02, col = "gray", border = "gray")
      text(xmax + 0.5, ymax / 2, srt = -90, label = bquote(k == .(kk)))
    }
    if (show_x_panel_lab) {
      rect(-0.5, ymax * 1.02, xmax + 0.3, ymax * 1.1, col = "gray", border = "gray")
      text(xmax / 2, ymax * 1.06, srt = 0, label = bquote(m == .(mm)))
    }
  }
  par(
    mfcol = c(length(k), length(m)),
    mai = c(0, 0, 0, 0),
    oma = c(4, 4.5, 0.5, 0.5),
    pty = "m"
  )
  for (mm in m) {
    for (kk in k) {
      if (plot_density_function) {
        plot_pdf(mm, kk,
          show_y_lab = (mm == min(m)), show_x_lab = (kk == max(k)),
          show_y_panel_lab = (mm == max(m)), show_x_panel_lab = (kk == min(k))
        )
        mtext(text = bquote(italic("x")), side = 1, line = 2.5, las = 1, outer = TRUE)
        mtext(
          text = bquote(bold("PDF (probability density function)" ~ italic(P(X == x[i])))),
          side = 2, at = 0.5, las = 3, line = 2.5, outer = TRUE
        )
      } else {
        plot_cdf(mm, kk,
          show_y_lab = (mm == min(m)), show_x_lab = (kk == max(k)),
          show_y_panel_lab = (mm == max(m)), show_x_panel_lab = (kk == min(k))
        )
        mtext(text = bquote(italic("x")), side = 1, line = 2.5, las = 1, outer = TRUE)
        mtext(
          text = bquote(bold("PDF (probability density function)" ~ italic(P(X <= x[i])))),
          side = 2, at = 0.5, las = 3, line = 2.5, outer = TRUE
        )
      }
    }
  }
}
# Plot_Hypergeometric_Density()
