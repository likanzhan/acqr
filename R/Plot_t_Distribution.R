#' Plot t distributon
#' @examples
#' Plot_t_Distribution(n=9, show_critical_region=T, fill_critical_region=T, show_normal_curve=F, data_points= 2.5)
#' @export

Plot_t_Distribution <- function(mean = 13,
                                sd = 1,
                                df = 14,
                                alpha_level = 0.05,
                                sigma_range = 6,
                                alternative = "two.sided", # greater, less
                                show_critical_region = FALSE,
                                fill_critical_region = FALSE,
                                fill_confidence_interval = FALSE,
                                show_normal_curve = TRUE,
                                show_x_value = FALSE,
                                data_points = NULL) {
 
  ##### 1. Colors used in the plotting
  plot_colors <- c("#008744", "#0057e7", "#d62d20", "#ffa700") # google

  zcolor <- "#008744" # ggsci::pal_aaas("default")(10)[2]
  tcolor <- "#d62d20" # ggsci::pal_aaas("default")(10)[3]
  
  cr_color <- "#ffa700"
  ci_color <- "#f4cdba"
  
  data_points_color <- "blue" #"darkgreen"

  ##### 2. Calculate the relevant values for the plotting
  degree_of_freedom <- df
  x_value_range <- seq(- sigma_range, sigma_range, by = 0.001)
  t_value_range <- dt(x_value_range, df = degree_of_freedom)
  z_value_range <- dnorm(x_value_range)
  t_value_max <- max(t_value_range)
  z_value_max <- max(z_value_range)

  if (alternative == "two.sided") {
  	alpha_value <- alpha_level / 2
  	
  	x_boundary_low  <- qt(p = alpha_level / 2, df = degree_of_freedom, lower.tail = TRUE)
  	x_boundary_high <- qt(p = alpha_level / 2, df = degree_of_freedom, lower.tail = FALSE)	
  	
  	label_position <- x_boundary_high
  	label_side <- 4  	
  	
    cr_left_x <- c(min(x_value_range), x_value_range[x_value_range <= x_boundary_low], x_boundary_low)
    cr_left_y <- dt(cr_left_x, df = degree_of_freedom)
    cr_left_y[c(1, length(cr_left_y))] <- 0
    
    cr_right_x <- c(x_boundary_high, x_value_range[x_value_range >= x_boundary_high], max(x_value_range))
    cr_right_y <- dt(cr_right_x, df = degree_of_freedom)
    cr_right_y[c(1, length(cr_right_y))] <- 0
    
    ci_range_x <- x_value_range[x_value_range >= x_boundary_low & x_value_range <= x_boundary_high]
    ci_range_y <- dt(ci_range_x, df = degree_of_freedom)
    ci_range_y[c(1, length(ci_range_y))] <- 0  	
    
  	x_boundary_low_z  <- qnorm(p = alpha_level / 2, lower.tail = TRUE)
  	x_boundary_high_z <- qnorm(p = alpha_level / 2, lower.tail = FALSE)	  
  	
  	label_position_z <- x_boundary_high_z
  	label_side_z <- 4    
  	
  } else if (alternative == "greater") {
  	alpha_value <- alpha_level  	
  	
  	x_boundary_low  <- - Inf
  	x_boundary_high <- qt(p = alpha_level, df = degree_of_freedom, lower.tail = FALSE)
  	
  	label_position <- x_boundary_high
  	label_side <- 4

    cr_left_x <- cr_left_y <- NULL 
    cr_right_x <- c(x_boundary_high, x_value_range[x_value_range >= x_boundary_high], max(x_value_range))
    cr_right_y <- dt(cr_right_x, df = degree_of_freedom)
    cr_right_y[c(1, length(cr_right_y))] <- 0  	
    
    ci_range_x <- x_value_range[x_value_range <= x_boundary_high]
    ci_range_y <- dt(ci_range_x, df = degree_of_freedom)
    ci_range_y[c(1, length(ci_range_y))] <- 0  
    
  	x_boundary_low_z  <- - Inf
  	x_boundary_high_z <- qnorm(p = alpha_level, lower.tail = FALSE)
  	
  	label_position_z <- x_boundary_high_z
  	label_side_z <- 4       	

  } else if (alternative == "less") {
  	alpha_value <- alpha_level
  	
  	x_boundary_low  <- qt(p = alpha_level, df = degree_of_freedom, lower.tail = TRUE)
  	x_boundary_high <- + Inf
  		
  	label_position <- x_boundary_low
  	label_side <- 2  	
  	
    cr_left_x <- c(min(x_value_range), x_value_range[x_value_range <= x_boundary_low], x_boundary_low)
    cr_left_y <- dt(cr_left_x, df = degree_of_freedom)
    cr_left_y[c(1, length(cr_left_y))] <- 0  
    
    cr_right_x <- cr_right_y <- NULL	
    
    ci_range_x <- x_value_range[x_value_range >= x_boundary_low]
    ci_range_y <- dt(ci_range_x, df = degree_of_freedom)
    ci_range_y[c(1, length(ci_range_y))] <- 0	
  	
   	x_boundary_low_z  <- qnorm(p = alpha_level, lower.tail = TRUE)
  	x_boundary_high_z <- + Inf  
  	
  	label_position_z <- x_boundary_low_z
  	label_side_z <- 2
 
  } else stop("alternative has to be: 'two.sided', 'less', or 'greater'")


  ##### 3. Do the plotting
  plot(x = NULL, y = NULL, type = "l", bty = "l", 
    yaxs = "i", axes = FALSE,
    xlim = c(min(x_value_range), max(x_value_range) + 1), 
    ylim = c(0, 1.2 * z_value_max),
    xlab = "", ylab = "Density", font.lab = 2
  )
  
  if (show_critical_region) {
    abline(v = c(x_boundary_low, x_boundary_high), col = tcolor, lty = 2)
    text(x = label_position, y = t_value_max / 1, adj = -0.05, col = tcolor,
         labels = bquote(italic(t) == .(label_position)), pos = label_side)
    text(x = 0, y = t_value_max / 2.5, 
    labels = bquote(italic(alpha / 2) == .(alpha_value)))
  }

  if (fill_critical_region) {
    polygon(x = cr_left_x,  y = cr_left_y,  border = "white", col = cr_color)
    polygon(x = cr_right_x, y = cr_right_y, border = "white", col = cr_color)
    }

  if (fill_confidence_interval) {
    polygon(x = ci_range_x, y = ci_range_y, border = "white", col = ci_color)
    segments(x_boundary_low,  0, x_boundary_low,  dt(0, df = degree_of_freedom) / 1.5)
    segments(x_boundary_high, 0, x_boundary_high, dt(0, df = degree_of_freedom) / 1.5)
    
    text(x = 0, y = dt(0, df = degree_of_freedom) / 10, pos = 3,
      label = bquote("Middle" ~ .(100 - alpha_level * 100) ~ "% of" ~ italic(t) ~ "distribution")
    )
  }

  lines(x_value_range, t_value_range, col = tcolor, lwd = 2)
  text(x = 0, y = t_value_max / 1.5, 
       labels = bquote(df == .(degree_of_freedom) ), col = tcolor)
  if (show_x_value) {
  text(x = 0, y = t_value_max / 2, 
       labels = bquote(atop(M == .(mean), s[m] == .(sd) )), col = tcolor) 
#       labels = bquote(atop(mu == .(mean), sigma == .(sd) )), col = tcolor)   	
  }

  axis(2, at = c(0, t_value_max + 0.2), labels = c("", ""), lwd.ticks = 0)
  axis(2, at = seq(0, t_value_max + 0.1, by = 0.1), lwd = 0, lwd.ticks = 1)
  axis(1, at = c(min(x_value_range) - 0.5, max(x_value_range) + 0.5), labels = c("", ""), lwd.ticks = 0)
  axis(1, at = seq(min(x_value_range), max(x_value_range), by = 1), lwd = 0, lwd.ticks = 1, line = 0)
  mtext("t value", side = 1, line = -0.5, adj = 1)
  if (show_x_value) {
  axis(1, at = c(min(x_value_range) - 0.5, max(x_value_range) + 0.5), labels = c("", ""), lwd.ticks = 0, line = 3)
#  axis(1, at = seq(min(x_value_range), max(x_value_range), by = 1), labels = mean + seq(min(x_value_range), max(x_value_range), by = 1) * sd, lwd = 0, lwd.ticks = 1, line = 3)
  axis(1, at = c(x_boundary_low, x_boundary_high), 
      labels = mean + round(c(x_boundary_low, x_boundary_high), 3) * sd, 
      lwd = 0, lwd.ticks = 1, line = 3, col.axis = tcolor)
  mtext("x value", side = 1, line = 2.5, adj = 1)  	
  }

  ##### 4. Add normal curve
  if (show_normal_curve) {
    lines(x_value_range, z_value_range, col = zcolor, lwd = 2)
    if (show_critical_region) {
      abline(v = c(x_boundary_low_z, x_boundary_high_z), col = zcolor, lty = 2)
      text(x = label_position_z, y = t_value_max / 4, adj = -0.05, col = zcolor,
        labels = bquote(italic(z) == .(label_position_z)), pos = label_side_z
      )
    }
  }
  
  if (show_normal_curve) {
    legend("top", lty = c(1, 1), col = c(zcolor, tcolor), bty = "n",
           legend = c("unit normal distribution", "t distribution")
           )
  } else legend("top", lty = 1, col = tcolor, bty = "n", legend = "t distribution")
  

  ###### add some data points
  if (!is.null(data_points)) {
    for (point in data_points) {
      mtext(bquote(t == ~.(point)), side = 1, at = point, line = 2, col = data_points_color)
      arrows(point, -dt(0, degree_of_freedom) / 7, point, 0, xpd = TRUE, length = 0.08, col = data_points_color)
    }
  }
}