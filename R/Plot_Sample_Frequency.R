Plot_Sample_Frequency <- function(
data,
show_sample_deviation = FALSE,
show_sample_grid = TRUE,
show_sample_sigma = FALSE,
AHZ = FALSE, AVD = TRUE,
show_population_curve = FALSE,
population_mean = mean(data),
population_sd = sd(data),
fill = "#fce6bf", border = "#b78135", 
xlab = "", ylab = "Frequency",
...
){
  ##### 1. Calculate the required value ######
  Table <- as.data.frame(table(data))
  Table[, "data"] <- as.numeric(as.character(Table[, "data"]))
  value <- Table[, "data"]
  freq <- Table[, "Freq"]
  width <- min(value[-1] - value[-length(value)])
  xlim <- c(min(value) -  width, max(value) + width)
  ylim <- c(0,  max(table(data)) + 1)
  breaks <- seq(min(value) - width / 2, max(value) + width / 2, by = width)
  xlabel <- round(seq(min(value), max(value), by = width), 2)
  xzlabel <- round((xlabel - population_mean) / population_sd, 2)
  ylabel <- seq(0, max(table(data)), by = 1)
  ydensitylabel <- format(ylabel / length(data), digits = 2)

  ##### 2. Do the plotting ######
  par(mar = c(5, 4, 4, 5) + 0.1)
  hist(data, ylim = ylim, xlim = xlim, breaks = breaks, 
    col = fill, border = border, main = "", xlab = xlab, ylab = ylab,
    yaxs = "i", xaxs = "i", xaxt = "n", yaxt = "n", ...)
  mtext(expression(italic("X")), side = 1, line = 0, adj = 0.99)
  axis(1, at = xlim, labels = c("", ""), lwd.ticks = 0)
  axis(1, at = xlabel, labels = xlabel, lwd = 0, lwd.ticks = 1, padj = -0.5)
  axis(2, at = ylim, labels = c("",""), lwd.ticks = 0)
  axis(2, at = ylabel, labels = ylabel, lwd = 0, lwd.ticks = 1, las = 1)

  ##### 3. Add more axes ######
  if (AVD){
    axis(4, at = ylim, labels = c("",""), lwd.ticks = 0)
    axis(4, at = ylabel, labels = ydensitylabel, lwd = 0, lwd.ticks = 1, las = 1)
    mtext("Density", side = 4, line = 4, padj = - 1)
  }

  if (AHZ) {
    axis(1, at = xlim, labels = c("", ""), lwd.ticks = 0, line = 2.5)
    axis(1, at = xlabel, labels = xzlabel, lwd = 0, lwd.ticks = 1, line = 2.5, padj = -0.5)
    mtext(expression(italic("z")), side = 1, line = 2.5, adj = 0.99)
  }

  ##### 4. Add more information concerning the sample data ######
  if (show_sample_grid){
    invisible(lapply(1:nrow(Table), function(i) {
    invisible(lapply(1:freq[i], function(j){
      segments(value[i]  - width / 2, 1:j, value[i]  + width / 2, 1:j, col = border)
              }))
      }))
    }

  if (show_sample_sigma) {
  	segment_y0 <- Table[Table[, "data"] == round(mean(data), 0), "Freq"]
  	segments(x0 = mean(data), y0 = segment_y0, x1 = mean(data), y1 = max(freq) + 2)
    arrows(mean(data)- sd(data), ylim[2]-0.5, mean(data) + sd(data), ylim[2] - 0.5, code = 3, length = 0.1)
    mtext(side = 3, at = mean(data), text = bquote(italic(M) == .(round(mean(data), 2))))
    text(x = mean(data) + sd(data) / 2, y = ylim[2] - 0.5, label = bquote(s == .(round(sd(data), 2))), pos = 3)
    text(x = mean(data) - sd(data) / 2, y = ylim[2] - 0.5, label = bquote(s == .(round(sd(data), 2))), pos = 3)
  }

  if (show_sample_deviation) {
    sdata <- data[data != population_mean]
    unique <- unique(sdata)
    length <- length(unique)
    segments(
      x0 = unique, y0 = as.numeric(table(sdata)),
      x1 = unique, y1 = seq(max(table(sdata)) + 0.1 , max(table(sdata)) + 0.9, length.out = length))
    segments(
      x0 = population_mean, y0 = length(data[data == population_mean]), x1 = population_mean, y1 = max(table(sdata)) + 1 )
    segments(
      x0 = unique, y0 = seq(max(table(sdata)) + 0.1 , max(table(sdata)) + 0.9, length.out = length),
      x1 = rep(population_mean, length), y1 = seq(max(table(sdata)) + 0.1 , max(table(sdata)) + 0.9, length.out = length),
         col = "red")
    mtext(side = 3, at = population_mean, text = bquote(italic(mu[0]) == .(population_mean)))	
    mtext(side = 3, at = mean(data), text = bquote(italic(M) == .(round(mean(data), 2))), line = 1)
  }

  ##### 5. Add population curve ######
  if (show_population_curve){
    zx <- seq(population_mean - 3 * population_sd, population_mean + 3 * population_sd, len = 100)
    zy <- dnorm(zx, mean = population_mean, sd = population_sd) * length(data)
    lines(zx, zy, col = "#cf232a", lwd = 2)
  }

}
