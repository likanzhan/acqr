Modified_Frequency_Distribution <- function(data, ...){
  Table <- as.data.frame(table(data))
  Table[, "data"] <- as.numeric(as.character(Table[, "data"]))
  ylim <- c(0, max(Table[, "Freq"]) + 1)
  xlim <- c(min(Table[, "data"]) - 1, max(Table[, "data"]) + 1)
  xlabel <- seq(min(Table[, "data"]), max(Table[, "data"]), by = 1)
  ylabel <- seq(0, max(Table[, "Freq"]), by = 1)

  hist(data, ylim = ylim, xlim = xlim,
    breaks = seq(min(Table[, "data"]) - 0.5, max(Table[, "data"]) + 0.5, by = 1),
    yaxs = "i", xaxs = "i", xaxt = "n", yaxt = "n", col = "#fce6bf", border = "#b78135", ...)
axis(1, at = xlim, labels = c("", ""), lwd.ticks = 0)
axis(1, at = xlabel, lwd = 0, lwd.ticks = 1)
axis(2, at = ylim, labels = c("",""), lwd.ticks = 0)
axis(2, at = ylabel, lwd = 0, lwd.ticks = 1)
invisible(lapply(1:nrow(Table), function(i) {
	invisible(lapply(1:Table[i, "Freq"], function(j){
		segments(Table[i, "data"]  - 0.5, 1:j, Table[i, "data"]  + 0.5, 1:j, col = "#b78135")
	}))
}))
}