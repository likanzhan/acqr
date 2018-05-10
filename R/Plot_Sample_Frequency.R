Plot_Sample_Frequency <- function(
data,
adjx = 1,
mean_sd = FALSE,
main = "", xlab = "",
AHZ = FALSE,
AVD = TRUE,
grid = TRUE,
fill = "#fce6bf",
border = "#b78135",
curve = FALSE,
curve_mean = mean(data),
curve_sd = sd(data),
...
){
  Table <- as.data.frame(table(data))
  Table[, "data"] <- as.numeric(as.character(Table[, "data"]))
  value <- Table[, "data"]
  freq <- Table[, "Freq"]
  width <- min(value[-1] - value[-length(value)])
  xlim <- c(min(value) -  width, max(value) + width)
  ylim <- c(0,  max(table(data)) + 0.5)
  breaks <- seq(min(value) - width / 2, max(value) + width / 2, by = width)
  xlabel <- round(seq(min(value), max(value), by = width), 2)
  ylabel <- seq(0, max(table(data)), by = 1)
  ydensitylabel <- format(ylabel / length(data), digits = 2)

par(mar = c(5.5, 5, 1, 6))
hist(data, ylim = ylim, xlim = xlim, breaks = breaks,
    yaxs = "i", xaxs = "i", xaxt = "n", yaxt = "n", col = fill, border = border,
    main = main, xlab = xlab, ...)
mtext(expression(italic("X")), side = 1, line = 0, adj = 0.99)
axis(1, at = xlim, labels = c("", ""), lwd.ticks = 0)
axis(1, at = xlabel, labels = xlabel, lwd = 0, lwd.ticks = 1)
axis(2, at = ylim, labels = c("",""), lwd.ticks = 0)
axis(2, at = ylabel, labels = ylabel, lwd = 0, lwd.ticks = 1, las = 1)

if (grid){
  invisible(lapply(1:nrow(Table), function(i) {
    invisible(lapply(1:freq[i], function(j){
      segments(value[i]  - width / 2, 1:j, value[i]  + width / 2, 1:j, col = border)
    }))
  }))
}

if (AVD){
  axis(4, at = ylim, labels = c("",""), lwd.ticks = 0)
  axis(4, at = ylabel, labels = ydensitylabel, lwd = 0, lwd.ticks = 1, las = 1)
  mtext("Density", side = 4, line = 4)
}

if (AHZ) {
axis(1, at = xlim, labels = c("", ""), lwd.ticks = 0, line = 2.5)
axis(1, at = xlabel, labels = round(scale(xlabel), 2), lwd = 0, lwd.ticks = 1, line = 2.5)
mtext(expression(italic("z")), side = 1, line = 2.5, adj = 0.99)
}

if (curve){
zx <- seq(curve_mean - 3 * curve_sd, curve_mean + 3 * curve_sd, len = 100)
zy <- dnorm(zx, mean = curve_mean, sd = curve_sd) * length(data)
lines(zx, zy, col = "#cf232a", lwd = 2)
}

if (mean_sd) {
abline(v = mean(data), lty=2)
abline(v= mean(data) + sd(data), lty = 2)
text(x = mean(data), y = 0.25, label = bquote(mu==~.(mean(data))), pos = 4)
arrows(mean(data), ylim[2]-0.5, mean(data) + sd(data), ylim[2] - 0.5, code = 3, length = 0.1)
text(x = mean(data) + sd(data) / 2, y = ylim[2] - 0.5, label = bquote(sigma==~.(round(sd(data), 2))), pos = 1)
}
}
