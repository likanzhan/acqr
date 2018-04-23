Modified_Frequency_Distribution <- function(
data, 
adjx = 1, 
mean_sd = FALSE,
main = "", xlab = "",
AZ = FALSE,
...
){
  Table <- as.data.frame(table(data))
  Table[, "data"] <- as.numeric(as.character(Table[, "data"]))
  ylim <- c(0, max(Table[, "Freq"]) + 1)
  xlim <- c(min(Table[, "data"]) - adjx, max(Table[, "data"]) + 1)
  xlabel <- seq(min(Table[, "data"]), max(Table[, "data"]), by = 1)
  ylabel <- seq(0, max(Table[, "Freq"]), by = 1)

par(mar = c(5, 4, 1, 1.5))
hist(data, ylim = ylim, xlim = xlim,
    breaks = seq(min(Table[, "data"]) - 0.5, max(Table[, "data"]) + 0.5, by = 1), 
    yaxs = "i", xaxs = "i", xaxt = "n", yaxt = "n", col = "#fce6bf", border = "#b78135",
    main = main, xlab = xlab, ...)
mtext(expression(italic("X")), side = 1, line = -0.5, adj = 1.02)
axis(1, at = xlim, labels = c("", ""), lwd.ticks = 0)
axis(1, at = xlabel, labels = xlabel, lwd = 0, lwd.ticks = 1)
axis(2, at = ylim, labels = c("",""), lwd.ticks = 0)
axis(2, at = ylabel, lwd = 0, lwd.ticks = 1)
invisible(lapply(1:nrow(Table), function(i) {
	invisible(lapply(1:Table[i, "Freq"], function(j){
		segments(Table[i, "data"]  - 0.5, 1:j, Table[i, "data"]  + 0.5, 1:j, col = "#b78135")
	}))
}))

if (AZ) {
axis(1, at = xlim, labels = c("", ""), lwd.ticks = 0, line = 2.5)
axis(1, at = xlabel, labels = round(scale(xlabel), 2), lwd = 0, lwd.ticks = 1, line = 2.5)
mtext(expression(italic("z")), side = 1, line = 2, adj = 1.02)	
}

if (mean_sd) {
abline(v = mean(data), lty=2)
abline(v= mean(data) + sd(data), lty = 2)
text(x = mean(data), y = 0.25, label = bquote(mu==~.(mean(data))), pos = 4)
arrows(mean(data), ylim[2]-0.5, mean(data) + sd(data), ylim[2] - 0.5, code = 3, length = 0.1)
text(x = mean(data) + sd(data) / 2, y = ylim[2] - 0.5, label = bquote(sigma==~.(round(sd(data), 2))), pos = 3)	
}
}