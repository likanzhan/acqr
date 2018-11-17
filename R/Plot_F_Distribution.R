#' Plot F Distribution
#' @export

Plot_F_Distribution <- function(
                                df1 = 1, df2 = 14, alpha_level = 0.05,
                                x_range = c(0, 7), y_range = NULL,
                                fill_critical_region = TRUE, show_t_curve = FALSE) {
  #### 1. Define colors to do the drawing
  fcolor <- "#d62d20" # ggsci::pal_aaas("default")(10)[3]
  ffill <- "#F6D5D2" # "#e9c4b2"
  tcolor <- "#008744" # ggsci::pal_aaas("default")(10)[2]
  tfill <- "#E5F3EC"

  #### 2. Calculate the relevant values
  x_t <- seq(min(x_range), max(x_range), by = 0.0001)
  y_t <- dt(x_t, df2)
  critical_region_t <- qt(alpha_level / 2, df2, lower.tail = FALSE)
  critical_region_t_x <- c(critical_region_t, x_t[x_t >= critical_region_t], max(x_t))
  critical_region_t_y <- c(0, dt(x_t[x_t >= critical_region_t], df2), 0)

  x_F <- x_t[x_t >= 0]
  y_F <- df(x_F, df1, df2)
  critical_region_F <- qf(alpha_level, df1, df2, lower.tail = FALSE)
  critical_region_F_x <- c(critical_region_F, x_F[x_F >= critical_region_F], max(x_F))
  critical_region_F_y <- c(0, df(x_F[x_F >= critical_region_F], df1, df2), 0)

  y_total <- c(y_t, y_F)
  y_total <- y_total[is.finite(y_total)]
  y_max <- ifelse(is.null(y_range), max(y_total), y_range)

  xlim <- c(min(x_range), max(x_range) + 0.5)
  ylim <- c(0, y_max * 1.1)

  ### 3. Do the plotting
  plot(NULL, NULL,
    xlim = xlim, ylim = ylim,
    type = "l", bty = "l", yaxs = "i", xaxs = "i", axes = FALSE,
    ylab = "Density", xlab = "t / F values", font.lab = 2
  )
  mtext(expression(italic("t/F")), side = 1, line = 0, adj = 0.99)
  axis(1, at = xlim, labels = c("", ""), lwd.ticks = 0)
  axis(1, at = seq(min(x_range), max(x_range)), lwd = 0, lwd.ticks = 1, padj = -0.5)
  axis(2)

  if (fill_critical_region) {
    polygon(x = critical_region_F_x, y = critical_region_F_y, col = ffill, border = NULL)
  }
  lines(x_F, y_F, col = fcolor, lwd = 2)
  abline(v = critical_region_F, col = fcolor, lty = 2)
  text(
    x = 1, y = y_max + 0.02, pos = 4, offset = -2, col = fcolor,
    labels = bquote(df[1] == .(df1) ~ "," ~ df[2] == .(df2))
  )
  text(
    x = critical_region_F, y = y_max / 10, col = fcolor, pos = 4, offset = 0.2,
    labels = bquote(italic(F == .(round(critical_region_F, 2))) ~ "," ~ italic(alpha) == .(alpha_level))
  )

  if (show_t_curve) {
    if (fill_critical_region) {
      polygon(x = critical_region_t_x, y = critical_region_t_y, col = tfill, border = NULL)
      polygon(x = -critical_region_t_x, y = critical_region_t_y, col = tfill, border = NULL)
    }
    lines(x_t, y_t, col = tcolor, lwd = 2)
    abline(v = critical_region_t, lty = 2, col = tcolor)
    text(
      x = critical_region_t, y = y_max / 3.5, pos = 4, col = tcolor, offset = 0.2,
      labels = bquote(italic(t == .(round(critical_region_t, 2))) ~ "," ~ italic(alpha / 2) == .(alpha_level / 2))
    )
  }
}

# Plot_F_Distribution(df1=1, df2=20, x_range = c(0, 7), show_t_curve = T, y_range=0.5)
