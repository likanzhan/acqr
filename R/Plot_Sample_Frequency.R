#' Plot the sample frequency
#' @param add_fill_color    Logical or a numeric value, To fill or not, or specifiy the cells to be filled.
#' @param sample1, sample2  numeric vectors to be drawn, sample2 can be NULL.
#' @examples
#' sample1 <- c(11, 9, 4, 5, 6, 7, 12, 10)
#' sample2 <- c(7, 13, 14, 16, 9, 11, 15, 11)
#' Plot_Sample_Frequency(sample1, sample2, show_sample_deviation = T, deviation_from_population_mean = F)
#' @export

Plot_Sample_Frequency <- function(
                                  sample1, 
                                  sample2 = NULL,
                                  x_range = NULL,
                                  show_sample_deviation = FALSE,
                                  show_sample_grid = TRUE,
                                  show_sample_sigma = FALSE,
                                  show_sample_variation_range = FALSE,
                                  standarized_x_axis = FALSE,
                                  standarized_y_axis = FALSE,
                                  show_population_curve = FALSE,
                                  population_mean = mean(c(sample1, sample2)),
                                  population_sd = sd(c(sample1, sample2)),
                                  deviation_from_population_mean = TRUE,
                                  add_fill_color = TRUE,
                                  xlab = "", 
                                  ylab = "Frequency", 
                                  main = "",
                                  ...) {
  ##### 0. Define colors ######
  sample1_col <- c("#f8cea2", "#b78135", "#cf232a")
  sample2_col <- c("#a0d5cf", "#009f9b")
  ######## 1. Calculate the required value ######

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
  sample_total <- c(sample1, sample2)
  sample_total_tidy <- sample_tidy(sample_total)
  sample_total_values_x <- sample_total_tidy[["Table"]][["data"]]
  sample_total_values_x <- sort(sample_total_values_x)
  sample_total_frequency_x <- sample_total_tidy[["Table"]][["Frequency"]]
  sample_total_unique_x <- sample_total_tidy[["Table_Unique"]][["data"]]
  sample_total_unique_x <- sort(sample_total_unique_x)
  sample_total_unique_frequency <- sample_total_tidy[["Table_Unique"]][["Frequency"]]
  sample_total_unique_length <- nrow(sample_total_tidy[["Table_Unique"]])
  sample_total_deviation_rank <- sample_total_tidy[["Table_Unique"]]$data_deviation_rank
  sample_total_deviation_rank_range <- 
      sample_total_tidy[["Table_Unique"]][sample_total_deviation_rank %in% range(sample_total_deviation_rank), ]
  sample_total_deviation_rank_range <- 
      sample_total_deviation_rank_range[!duplicated(sample_total_deviation_rank_range $ data_deviation_sign), ]
  sample_total_deviation_rank_range <- 
      sample_total_deviation_rank_range[order(sample_total_deviation_rank_range $ data_deviation_rank), ]
  sample_1_tidy <- sample_tidy(sample1)
  sample_1_values_x <- sample_total_values_x[sample_total_values_x %in% sample1]
  sample_1_values_frequency <- 
      sample_total_tidy[["Table"]][sample_total_tidy[["Table"]][["data"]] %in% sample_1_values_x, "Frequency"]
  sample_1_valus_length <- length(sample_1_values_x)

  sample_2_tidy <- if (!is.null(sample2)) sample_tidy(sample2) else sample_1_tidy
  sample_2_values_x <- sample_total_values_x[sample_total_values_x %in% sample2]
  sample_2_values_frequency <- 
      sample_total_tidy[["Table"]][sample_total_tidy[["Table"]][["data"]] %in% sample_2_values_x, "Frequency"]
  sample_2_valus_length <- length(sample_2_values_x)

  bin_width <- min(sample_total_values_x[-1] - sample_total_values_x[-length(sample_total_values_x)])

  if (is.null(x_range)) {
    xlim <- c(min(sample_total_values_x) - bin_width, max(sample_total_values_x) + bin_width)
  } else {
    xlim <- c(min(x_range) - bin_width, max(x_range) + bin_width)
  }
  ylim <- c(0, max(sample_total_frequency_x) + 1)
  breaks <- seq(min(xlim) + bin_width / 2, max(xlim) + bin_width / 2, by = bin_width)
  xlabel <- round(seq(min(xlim) + bin_width, max(xlim) - bin_width, by = bin_width), 2)
  xzlabel <- round((xlabel - population_mean) / population_sd, 2)
  ylabel <- seq(0, max(sample_total_frequency_x), by = 1)
  ydensitylabel <- format(ylabel / length(sample_total), digits = 2)
  ##### 2. Do the plotting ######
  par(mar = c(5, 4, 4, 5) + 0.1)
  hist(sample_total,
    ylim = ylim, xlim = xlim, breaks = breaks, font.lab = 2,
    col =  ifelse(is.logical(add_fill_color) && add_fill_color, sample1_col[1], "white"), 
    border = sample1_col[2], main = main, xlab = xlab, ylab = ylab,
    yaxs = "i", xaxs = "i", xaxt = "n", yaxt = "n", ...
  )
  mtext(expression(italic("X")), side = 1, line = 0, adj = 0.99)
  axis(1, at = xlim, labels = c("", ""), lwd.ticks = 0)
  axis(1, at = xlabel, labels = xlabel, lwd = 0, lwd.ticks = 1, padj = -0.5)
  axis(2, at = ylim, labels = c("", ""), lwd.ticks = 0)
  axis(2, at = ylabel, labels = ylabel, lwd = 0, lwd.ticks = 1, las = 1)

  ##### 3. Add more axes ######
  if (standarized_y_axis) { # AXZ
    axis(4, at = ylim, labels = c("", ""), lwd.ticks = 0)
    axis(4, at = ylabel, labels = ydensitylabel, lwd = 0, lwd.ticks = 1, las = 1)
    mtext("Density", side = 4, line = 4, padj = -1)
  }

  if (standarized_x_axis) { # AYD
    axis(1, at = xlim, labels = c("", ""), lwd.ticks = 0, line = 2.5)
    axis(1, at = xlabel, labels = xzlabel, lwd = 0, lwd.ticks = 1, line = 2.5, padj = -0.5)
    mtext(expression(italic("z")), side = 1, line = 2.5, adj = 0.99)
  }

  ##### 4. Add more information concerning the sample data ######
  plot_grids <- function(tidy, frequency, values, color) {
    invisible(lapply(1:nrow(tidy[["Table"]]), function(i) {
      invisible(lapply(1:frequency[i], function(j) {
        segments(values[i] - bin_width / 2, 1:j, values[i] + bin_width / 2, 1:j, col = color)
      }))
    }))
  }

  if (!is.null(sample2)) {
    hist(x = sample1, breaks = breaks, col = sample2_col[1], border = sample2_col[2], add = TRUE)
    legend("topleft", lwd = 10, col = c(sample1_col[1], sample2_col[1]), bty = "n", legend = c("Condition A", "Condition B"))
  }
  
  if (!is.logical(add_fill_color)) {
     values_to_be_colored = sample_total[sample_total >= add_fill_color]
     hist(x = values_to_be_colored, breaks = breaks, col = sample1_col[1], border = sample1_col[2], add = TRUE)
  }
  
  if (show_sample_grid) {
    if (is.null(sample2)) {
      plot_grids(sample_total_tidy, sample_total_frequency_x, sample_total_values_x, sample1_col[2])
    } else {
      plot_grids(sample_total_tidy, sample_total_frequency_x, sample_total_values_x, sample2_col[2])
      plot_grids(sample_2_tidy, sample_2_values_frequency, sample_2_values_x, sample1_col[2])
    }
  }

  ##### Add legends if two sampels are included
  sample_sigma <- function(sample_total, y_adj = 0.5, color = sample1_col[2]) {
    segments(x0 = mean(sample_total), y0 = ylim[2] - 1, 
             x1 = mean(sample_total), y1 = max(sample_total_frequency_x) + 1, col = color)
    arrows(mean(sample_total) - sd(sample_total), ylim[2] - y_adj, mean(sample_total) + sd(sample_total), 
        ylim[2] - y_adj, code = 3, length = 0.1, col = color)
    mtext(side = 3, at = mean(sample_total), text = bquote(italic(M) == .(round(mean(sample_total), 2))), col = color)
    text(x = mean(sample_total) + sd(sample_total) / 2, y = ylim[2] - y_adj - 0.05, 
        label = bquote(s == .(round(sd(sample_total), 2))), pos = 3, col = color)
    text(x = mean(sample_total) - sd(sample_total) / 2, y = ylim[2] - y_adj - 0.05, 
        label = bquote(s == .(round(sd(sample_total), 2))), pos = 3, col = color)
  }

  if (show_sample_sigma) {
    if (is.null(sample2)) {
      sample_sigma(sample1, y_adj = 0.5, color = sample1_col[2])
    } else {
      sample_sigma(sample1, y_adj = 0.6, color = sample2_col[2])
      sample_sigma(sample2, y_adj = 0.3, color = sample1_col[2])
    }
  }

  if (show_sample_variation_range) {
  	segments(sample_total_deviation_rank_range[, "data"], sample_total_deviation_rank_range[, "Frequency"], 
  	         sample_total_deviation_rank_range[, "data"], max(sample_total_frequency_x) + 0.5, 
  	         col = sample1_col[3], lwd = 2, lty = "dashed") 
  	abline(v = mean(sample_total), col = sample1_col[3], lwd = 2)
  	arrows(mean(sample_total), max(sample_total_frequency_x) + 0.5, 
  	       sample_total_deviation_rank_range[, "data"], ,
  	       col = sample1_col[3], lwd = 2, length = 0.1, code = 3 ) 
    mtext(side = 3, at = mean(sample_total), text = bquote(italic(M) == .(round(mean(sample_total), 2))), col = sample1_col[3])
    text(x = (sample_total_deviation_rank_range[, "data"] + mean(sample_total)) / 2, 
         y = max(sample_total_frequency_x) + 0.5, 
         label = round(abs(sample_total_deviation_rank_range[, "data"] - mean(sample_total)), 2), pos = 3, col = sample1_col[3])   
  }

  add_vertical_horizontal_lines <- function(sample, center = mean(sample), values, frequency, length, color, start, end) {
    ## vertical line above mu
    segments(x0 = center, y0 = length(frequency[frequency == center]), y1 = max(frequency) + 1, col = "gray")
    ## vertical lines above each unique observation
    segments(x0 = values, y0 = frequency, y1 = seq(max(frequency) + start, max(frequency) + end, length.out = length), col = "gray")
    ## horizontal lines connecting the vertical lines and the population mean
    segments(x0 = values, y0 = seq(max(frequency) + start, max(frequency) + end, length.out = length), 
        x1 = rep(center, sample_total_unique_length), col = color)
    ##
    mtext(side = 3, at = mean(sample), text = bquote(italic(M) == .(round(mean(sample), 2))))
  }

  add_lines_together <- function(center1, center2) {
    break_point <- (sample_1_valus_length / sample_total_unique_length) * 0.8
    if (is.null(sample2)) {
      add_vertical_horizontal_lines(sample = sample1, center = center1, values = sample_1_values_x, 
          frequency = sample_1_values_frequency, length = sample_1_valus_length, color = "red", start = 0.1, end = break_point)
    } else {
      add_vertical_horizontal_lines(sample1, center1, sample_1_values_x, sample_1_values_frequency, 
          sample_1_valus_length, color = "blue", 0.1, break_point)
      add_vertical_horizontal_lines(sample2, center2, sample_2_values_x, sample_2_values_frequency, 
          sample_2_valus_length, color = "red", break_point, 0.9)
    }
  }

  if (show_sample_deviation) {
    if (deviation_from_population_mean) {
      add_lines_together(population_mean, population_mean)
      mtext(side = 3, at = population_mean, text = bquote(italic(mu[0]) == .(population_mean)), line = 1)
    } else {
      add_lines_together(mean(sample1), mean(sample2))
      mtext(side = 3, at = population_mean, text = bquote(italic(mu[0]) == .(population_mean)), line = 1)
    }
  }

  ##### 5. Add population curve ######
  if (show_population_curve) {
    zx <- seq(population_mean - 3 * population_sd, population_mean + 3 * population_sd, len = 100)
    zy <- dnorm(zx, mean = population_mean, sd = population_sd) * length(sample1)
    lines(zx, zy, col = "#cf232a", lwd = 2)
  }
}