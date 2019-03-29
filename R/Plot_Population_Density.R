#' Plot density distributions of two populations
#' @export

####### Plot_Population_Density_with_Two_Distributions
Plot_Population_Density <- function(
                                    mean_null = 0,
                                    mean_alternative = mean_null + 4,
                                    sigma = 1,
                                    sigma_range = 4,
                                    x_range = NULL,
                                    n = 1,
                                    alpha_level = 0.05,
                                    two_tails = TRUE,
                                    show_alpha_level = TRUE,
                                    show_hypothesis_null = TRUE, 
                                    show_correct_reject = TRUE, 
                                    show_false_alarm = TRUE,
                                    show_hypothesis_alternative = TRUE, 
                                    show_hit = TRUE, 
                                    show_miss = TRUE,
                                    show_sigma_value = TRUE,
                                    show_sigma_size = FALSE,
                                    add_fill_color = TRUE,
                                    show_decision = TRUE,
                                    show_axis_x = TRUE,
                                    show_axis_x_label = TRUE,
                                    show_axis_z = FALSE,
                                    show_axis_z_label = FALSE,
                                    data_points = NULL,
                                    color_fill_area = NULL,
                                    color_fill_area_label = NULL
                                    ) {
  #### Define the colors
  plot_colors <- c("#008744", "#0057e7", "#d62d20", "#ffa700") # google
  # plot_colors <- c("#008744", "#0057e7", "#d62d20", "#ffa700")

  col2alpha <- function(col, alpha) {
    col_rgb <- col2rgb(col) / 255
    rgb(col_rgb[1], col_rgb[2], col_rgb[3], alpha = alpha)
  }

  #### Calculate critical values
  sigma_m <- sigma / sqrt(n)
  d <- (mean_alternative - mean_null) / sigma
  ### X and Ys
  X0s <- seq(mean_null - sigma_range * sigma_m, mean_null + sigma_range * sigma_m, by = 0.01 * sigma_m)
  X1s <- seq(mean_alternative - sigma_range * sigma_m, mean_alternative + sigma_range * sigma_m, by = 0.01 * sigma_m)
  Xs <- sort(cbind(X0s, X1s))
  if (is.null(x_range)) Xs <- Xs else Xs <- seq(min(x_range), max(x_range), by = 0.01 * sd(x_range))
  Y0s <- dnorm(Xs, mean = mean_null, sd = sigma_m)
  Y1s <- dnorm(Xs, mean = mean_alternative, sd = sigma_m)

  #### Calculate values for Null Hypothesis
  if (two_tails) {
    X_min <- qnorm(alpha_level / 2, mean = mean_null, sd = sigma_m, lower.tail = TRUE)
    X_max <- qnorm(alpha_level / 2, mean = mean_null, sd = sigma_m, lower.tail = FALSE)
    low_tail <- seq(min(min(Xs), X_min), max(min(Xs), X_min), by = sigma_m * 0.001)
    high_tail <- seq(X_max, max(Xs), by = sigma_m * 0.001)
    alpha_value <- sub("^0\\.", ".", formatC(alpha_level / 2, digits = 2, format = "fg"))
    correct_rject_value <- sub("^0\\.", ".", formatC(1 - alpha_level, digits = 2, format = "fg"))
    boddy <- seq(X_min, X_max, by = sigma_m * 0.001)
    tail_x <- c(min(low_tail), low_tail, max(low_tail), min(high_tail), high_tail, max(high_tail))
    tail_y <- c(
      0, dnorm(low_tail, mean = mean_null, sd = sigma_m), 0,
      0, dnorm(high_tail, mean = mean_null, sd = sigma_m), 0
    )
  } else {
    X_max <- qnorm(alpha_level, mean = mean_null, sd = sigma_m, lower.tail = FALSE)
    high_tail <- seq(X_max, max(Xs), by = sigma_m * 0.001)
    alpha_value <- sub("^0\\.", ".", formatC(alpha_level, digits = 2, format = "fg"))
    correct_rject_value <- sub("^0\\.", ".", formatC(1 - alpha_level, digits = 2, format = "fg"))
    boddy <- seq(min(Xs), X_max, by = sigma_m * 0.001)
    tail_x <- c(min(high_tail), high_tail, max(high_tail))
    tail_y <- c(0, dnorm(high_tail, mean = mean_null, sd = sigma_m), 0)
  }

  tail_col <- col2alpha(plot_colors[1], alpha = 0.5)
  body_x <- c(min(boddy), boddy, max(boddy))
  body_y <- c(0, dnorm(boddy, mean = mean_null, sd = sigma_m), 0)
  body_col <- col2alpha(plot_colors[2], alpha = 0.5)

  #### Calculate values for Alternative Hypothesis
  beta_value <- pnorm(q = high_tail, mean = mean_alternative, sd = sigma_m)
  beta_value <- formatC(beta_value, digits = 2, format = "fg")
  beta_value <- sub("^0\\.", ".", beta_value)
  power_value <- pnorm(q = high_tail, mean = mean_alternative, sd = sigma_m, lower.tail = FALSE)
  power_value <- formatC(power_value, digits = 2, format = "f")
  power_value <- sub("^0\\.", ".", power_value)
  beta_range <- seq(min(min(X1s), X_max), max(min(X1s), X_max), by = sigma_m * 0.001)
  beta_x <- c(min(X1s), beta_range, X_max)
  beta_y <- c(0, dnorm(beta_range, mean = mean_alternative, sd = sigma_m), 0)
  beta_col <- col2alpha(plot_colors[3], alpha = 0.5)
  power_range <- seq(X_max, max(X1s), by = sigma_m * 0.001)
  power_x <- c(X_max, power_range, max(X1s))
  power_y <- c(0, dnorm(power_range, mean = mean_alternative, sd = sigma_m), 0)
  power_col <- col2alpha(plot_colors[4], alpha = 0.5)

  Xss <- c(Xs, data_points)
  label_pos <- seq(min(Xss), max(Xss), by = sigma_m)
  #label_pos <- unique(c(label_pos, data_points))

  ######## Do the plot
  plot(NULL, NULL,
    xlim = range(Xs),
    ylim = c(0, (1 + 0.01) * max(Y0s)),
    axes = FALSE, xlab = "", ylab = "", yaxs = "i"
  )

  ####### Plot the null hypothesis
  if (show_hypothesis_null) {
    if (show_false_alarm & add_fill_color) {
      polygon(tail_x, tail_y, col = tail_col, border = "white")
      if (show_alpha_level) {
        if (two_tails) {
          text(
            x = X_min, y = dnorm(mean_null, mean_null, sd = sigma_m) / 15,
            label = bquote(alpha / 2 == .(alpha_value)), pos = 2
          )
        } else {
          text(
            x = X_max, y = dnorm(mean_null, mean_null, sd = sigma_m) / 15,
            label = bquote(alpha == .(alpha_value)), pos = 4
          )
        }
      }
    }
    if (show_correct_reject & add_fill_color) {
      polygon(body_x, body_y, col = body_col, border = "white")
      if (show_alpha_level) {
        text(
          x = mean_null - sigma_m * 0.6, y = dnorm(mean_null, mean_null, sd = sigma_m) / 3,
          bquote(1 - alpha == .(correct_rject_value)), pos = 3
        )
      }
    }
    lines(Xs, Y0s, col = "#cf232a", lwd = 2)
    if (show_sigma_value) {
      abline(v = mean_null, lwd = 1)
      arrows(
        x0 = mean_null, y0 = dnorm(mean_null + sigma_m, mean_null, sigma_m),
        x1 = mean_null + sigma_m, y1 = dnorm(mean_null + sigma_m, mean_null, sigma_m),
        length = 0.1
      )
      if (show_axis_x | (!show_axis_x & !show_axis_z)) sigma_mm <- sigma_m else sigma_mm <- 1
      text(
        x = mean_null + sigma_m / 2, y = dnorm(mean_null + sigma_m, mean_null, sigma_m), pos = 1,
        labels = if (n == 1) {
          bquote(sigma == .(round(sigma_mm, 2)))
        } else {
          bquote(sigma[M] == .(round(sigma_mm, 2)))
        }
      )
      if (show_hypothesis_alternative) mtext(expression(H[0]), side = 3, at = mean_null)
     mtext(bquote(mu == .(round(mean_null, 2))), side = 1, at = mean_null, col = "gray")
    }
  }

  ###### Plot the alternative hypothesis
  if (show_hypothesis_alternative) {
    if (show_miss & add_fill_color) {
      polygon(beta_x, beta_y, col = beta_col, border = "white")
      if (show_alpha_level) {
        text(
          x = X_max, y = dnorm(mean_alternative, mean_alternative, sd = sigma_m) / 15,
          labels = bquote(beta == .(beta_value)), pos = 2
        )
      }
    }
    if (show_hit & add_fill_color) {
      polygon(power_x, power_y, col = power_col, border = "white")
      if (show_alpha_level) {
        text(
          x = X_max + abs(mean_alternative - X_max) * 0.5,
          y = dnorm(mean_alternative, mean_alternative, sd = sigma_m) / 3,
          labels = bquote(1 - beta == .(power_value)), pos = 4
        )
      }
    }
    lines(Xs, Y1s, col = "#d5493a", lwd = 2)
    if (show_sigma_value) {
      abline(v = mean_alternative, lwd = 1, col = "gray")
      mtext(expression(H[1]), side = 3, at = mean_alternative)
      mtext(text = bquote("Cohen's d" == .(d) ~ ", n" == .(n)), side = 3, at = (mean_null + mean_alternative) / 2, padj = -2)
    }
  }

  ####### X-Axes and Z-Axes
  show_axis <- function(Baseline = 2.5, Axis = "z", show_label = TRUE) {
    llabel <- if (Axis == "z") {
      round((label_pos - mean_null) / sigma_m, 2)
    } else {
      round(label_pos, 2)
    }
    axis(1, at = c(min(label_pos) - 0.5 * sigma_m, max(label_pos) + 0.5 * sigma_m), labels = c("", ""), lwd.ticks = 0, line = Baseline)
    if (show_label) {
      axis(1, at = label_pos, label = rep("", length(label_pos)), lwd = 0, lwd.ticks = 1, line = Baseline)
      axis(1, at = label_pos, label = llabel, lwd = 0, lwd.ticks = 0, line = Baseline - 0.5)
    }
      mtext(text = bquote(italic(.(Axis))), side = 1, adj = 1.03, line = Baseline - 0.6)
  }

  if (show_axis_x) {
    if (n == 1) show_axis(0, "X", show_label = show_axis_x_label) else show_axis(0, "M", show_label = show_axis_x_label)
    if (show_axis_z) show_axis(2.5, "z", show_label = show_axis_z_label)
  } else {
    if (show_axis_z) show_axis(0, "z", show_label = show_axis_z_label) else show_axis(0, "z", show_label = show_axis_z_label)
  }
  ####### Add an area
  color_fill_area_function <- function(color_fill_area_element, color_fill_area_label) {
    p1 <- color_fill_area_element[1]
    p2 <- color_fill_area_element[2]
    cnvtp <- function(pp) if (pp == Inf) mean_null + sigma_range * sigma_m else if (pp == -Inf) mean_null - sigma_range * sigma_m else pp
    if (!is.na(p1) & !is.na(p2)) {
      pt1 <- cnvtp(p1)
      pt2 <- cnvtp(p2)
      polygon(c(pt1, pt1, seq(pt1, pt2, length.out = 1000), pt2, pt2),
        c(0, dnorm(pt1, mean_null, sigma_m), dnorm(seq(pt1, pt2, length.out = 1000), mean_null, sigma_m), dnorm(pt2, mean_null, sigma_m), 0),
        col = col2alpha("#f4cdba", alpha = 0.8), border = "#af1e23", lwd = 2
      ) ## col2alpha is defined in pPower.R
      percent <- function(x, digits = 2, format = "f", ...) {
        paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
      }
      area_size <- if (p1 == -Inf) {
        pnorm(p2, mean_null, sigma_m)
      } else if (p2 == +Inf) {
        pnorm(p1, mean_null, sigma_m, lower.tail = FALSE)
      } else {
        pnorm(p2, mean_null, sigma_m) - pnorm(p1, mean_null, sigma_m)
      }
      area_size <- percent(area_size, 2)
      labell <- ifelse(is.null(color_fill_area_label), area_size, color_fill_area_label)
      text(x = mean(c(pt1, pt2)), y = dnorm(mean_null + 2 * sigma_m, mean_null, sigma_m), label = labell, pos = 1, col = "blue")
      if ((!show_axis_x_label) | (!show_axis_x)) {
      	for (point in color_fill_area_element[is.finite(color_fill_area_element)]) mtext(round(point, 2), side = 1, at = point, col = "blue")
      	}
      if (show_axis_z && show_axis_x && !show_axis_x_label) {
      	for (point in color_fill_area_element[is.finite(color_fill_area_element)]) {
      		mtext(round(point, 2), side = 1, at = point, col = "blue")
            axis(1, at = point, label = "", lwd = 0, lwd.ticks = 1, line = 2.5) 
            axis(1, at = point, label = round((point - mean_null) / sigma_m, 2), lwd = 0, lwd.ticks = 0, line = 2, col = "blue",col.axis = "blue")    		
      		}
      	}
    }  	
  }
  
  if (is.list(color_fill_area)) {
  	for (i in 1:length(color_fill_area)) {
  		color_fill_area_function(color_fill_area[[i]], color_fill_area_label)
  	}
  } else if (!is.null(color_fill_area)) {
        color_fill_area_function(color_fill_area, color_fill_area_label)
  }

  ###### add some data points
  if (!is.null(data_points)) {
    for (point in data_points) {
      mtext(bquote(X == ~.(point)), side = 1, at = point, line = if (show_axis_z) 4 else 2, col = "blue")
      arrows(point, -dnorm(mean_null + sigma_m, mean_null, sigma_m) / (if (show_axis_z) 3.5 else 8.5), point, 0, xpd = TRUE, length = 0.08, col = "blue")
    }
  }

  ####### show the decision
  if (show_decision) {
    abline(v = X_max, lty = 3)
    text(
      x = X_max, y = dnorm(mean_null, mean_null, sd = sigma_m) / 1.1,
      label = expression(paste("Reject ", H[0])), pos = 4
    )
    arrows(
      x0 = X_max, y0 = dnorm(mean_null, mean_null, sd = sigma_m) / 1.16,
      x1 = X_max + sigma_m, length = 0.1
    )
    if (two_tails) {
      abline(v = X_min, lty = 3)
      text(
        x = X_min, y = dnorm(mean_null, mean_null, sd = sigma_m) / 1.1,
        label = expression(paste("Reject ", H[0])), pos = 2
      )
      arrows(
        x0 = X_min, y0 = dnorm(mean_null, mean_null, sd = sigma_m) / 1.16,
        x1 = X_min - sigma_m, length = 0.1
      )
    }
  }

  ####### Add the arrow showing the standard errors
  if (show_sigma_size) {
    segments(
      x0 = seq(mean_null - 2 * sigma_m, mean_null + 2 * sigma_m, by = sigma_m), y0 = 0,
      y1 = c(
        rep(dnorm(mean_null + sigma_m, mean_null, sigma_m), 2), dnorm(mean_null, mean_null, sigma_m),
        rep(dnorm(mean_null + sigma_m, mean_null, sigma_m), 2)
      ), lty = 5
    )
    arrows(
      x0 = c(mean_null, mean_null, mean_null + sigma_m, mean_null - sigma_m), 
      y0 = dnorm(mean_null + sigma_m, mean_null, sigma_m),
      x1 = c(mean_null + sigma_m, mean_null - sigma_m, mean_null + 2 * sigma_m, mean_null - 2 * sigma_m), length = 0.08
    )
  }
}

###########################################
#' Plot density distributions of one population
#' @export
Plot_Population_Density_Single <- function(
    m = 0, s = 1, p = NULL, 
    show_sigma_size = TRUE, 
    show_decision = FALSE, 
    two_tails = TRUE, 
    alpha_level = 0.05,  
    show_alpha_level = FALSE,
    show_false_alarm = FALSE,
    show_correct_reject = FALSE,
    ...
    ){
	Plot_Population_Density(
    mean_null = m, mean_alternative = m, sigma = s, data_point = p, 
    show_hypothesis_alternative = FALSE, two_tails = two_tails, alpha_level = alpha_level, show_alpha_level = show_alpha_level, 
    show_decision = show_decision, show_false_alarm = show_false_alarm, show_correct_reject = show_correct_reject, show_sigma_size = show_sigma_size, ...)	
}