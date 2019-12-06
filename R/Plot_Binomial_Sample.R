#' Create a Binomial Frequency Table Based on the Number of Toss in a Binomial Distribution
#' @export
Create_Binomial_Table <- function(n) {
  ln <- list(0:1)
  rn <- rep(ln, n)
  eg <- expand.grid(rn)
  colnames(eg) <- paste("Toss", 1:n, sep = "-")
  rs <- rowSums(eg)
  egg <- eg
  egg[egg == 0] <- "Tail"
  egg[egg == 1] <- "Head"
  egg[, "Number-of-Heads"] <- rs
  egg[, "Probability"] <- 1 / nrow(eg)
  egg <- xtable::xtable(egg, digits = c(rep(0, n + 2), 4))
  res <- list(egg, rs)
  return(res)
}

#' Plot The Sample Frequency of a Binomial Distribution
#' @param number_of_toss    Number of toss
#' @param show_density      To show the density
#' @param show_smooth_curve To show the normal smooth curve

#' @export

Plot_Binomial_Sample <- function(
                                 number_of_toss,
                                 show_density = TRUE,
                                 show_smooth_curve = FALSE,
                                 ...) {
  #### 1. Define Colors
  fill_color <- "#f8cea2"
  border_color <- "#b78135"
  smooth_curve_color <- "#cf232a"

  #### 2. Calculate some values
  freq <- Create_Binomial_Table(number_of_toss)[[2]]
  sample_tidy <- function(sample) {
    Mean <- mean(sample)
    Table <- as.data.frame(table(sample, dnn = "data"))
    names(Table)[names(Table) == "Freq"] <- "Frequency"
    Table[, "data"] <- as.numeric(as.character(Table[, "data"]))
    Table[, "data_deviation_rank"] <- rank(abs(scale(Table[, "data"])))
    Table[, "data_deviation_sign"] <- sign(scale(Table[, "data"]))
    Table_Unique <- Table[Table[, "data"] != Mean, ]
    res <- list(Table = Table, Table_Unique = Table_Unique, Mean = Mean)
    return(res)
  }
  sample_total_tidy <- sample_tidy(freq)
  sample_total_values_x <- sample_total_tidy[["Table"]][["data"]]
  sample_total_values_x <- sort(sample_total_values_x)
  sample_total_frequency_x <- sample_total_tidy[["Table"]][["Frequency"]]
  bin_width <- min(sample_total_values_x[-1] - sample_total_values_x[-length(sample_total_values_x)])

  plot_grids <- function(tidy, frequency, values, color) {
    invisible(lapply(1:nrow(tidy[["Table"]]), function(i) {
      invisible(lapply(1:frequency[i], function(j) {
        segments(values[i] - bin_width / 2, 1:j, values[i] + bin_width / 2, 1:j, col = color)
      }))
    }))
  }
  min <- min(freq) - 0.5
  max <- max(freq) + 0.5
  x <- seq(-0.5, number_of_toss + 0.5, by = 0.01)
  y <- dnorm(x, number_of_toss * 0.5, sqrt(number_of_toss * 0.5 * (1 - 0.5))) * (2^number_of_toss)

  #### 3. Do the plotting
  par(oma = c(0, 0, 0, 3))
  hist(
    freq,
    breaks = seq(min, max, by = 1),
    ylim = c(0, ceiling(max(y))),
    yaxs = "i", col = fill_color, border = border_color,
    axes = FALSE, xlab = "Number of Heads", ylab = "Frequency",
    main = "", font.lab = 2,
    ...
  )
  y_axis_label <- unique(round(axTicks(2), 0))
  axis(1, at = c(min - 1, max + 1), labels = c("", ""), lwd = 1, lwd.ticks = 0)
  axis(1, at = seq(min + 0.5, max - 0.5), labels = seq(min + 0.5, max - 0.5), lwd = 0, lwd.ticks = 1)
  axis(2, at = c(0, ceiling(max(y)) + 2), labels = c("", ""), lwd = 1, lwd.ticks = 0)
  axis(2, at = y_axis_label, labels = y_axis_label, lwd = 0, lwd.ticks = 1)
  plot_grids(sample_total_tidy, sample_total_frequency_x, sample_total_values_x, border_color)

  if (show_density) {
    axis(4, at = c(0, ceiling(max(y)) + 2), labels = c("", ""), lwd = 1, lwd.ticks = 0)
    axis(4, at = y_axis_label, labels = formatC(y_axis_label / (2^number_of_toss), 4, format = "f"), las = 2)
    mtext(text = "Probability", side = 4, line = 4, font = 2, las = 3)
  }

  if (show_smooth_curve) {
    lines(x, y, col = smooth_curve_color, lwd = 2)
  }
}
