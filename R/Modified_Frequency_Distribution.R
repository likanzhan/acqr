Modified_Frequency_Distribution <- function(
  data,
  color = "#fce6bf", border = "#b78135",
  xlab = "Attractiveness rating",
  main = deparse(substitute(data))
  ){
  Table <- as.data.frame(table(data))
  Table[, "data"] <- as.numeric(as.character(Table[, "data"]))
  hist(data,
    ylim = c(min(Table[, "Freq"]) - 1, max(Table[, "Freq"]) + 0.5),
    xlim = c(min(Table[, "data"]) - 1, max(Table[, "data"]) + 1),
    breaks = seq(min(Table[, "data"]) - 0.5, max(Table[, "data"]) + 0.5, by = 1),
    col = color, border = border, yaxs = "i", xaxs = "i",
    ylab = "Frequncy", xlab = xlab, main = main)
invisible(lapply(1:nrow(Table), function(i) {
	invisible(lapply(1:Table[i, "Freq"], function(j){
		segments(Table[i, "data"]  - 0.5, 1:j, Table[i, "data"]  + 0.5, 1:j, col = border)
	}))
}))
}
