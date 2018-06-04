####### Plot_Population_Density_with_Two_Distributions
Plot_Population_Density <- function (
  m0 = 0, 
  m1 = 4, 
  sigma = 1, 
  sigma_range = 4, 
  x_range = NULL,
  n = 1, 
  alpha_level = 0.05,
  two_tails = TRUE,
  show_alpha_level = TRUE,
  null_hypothesis = TRUE, correct_reject = TRUE, false_alarm = TRUE,
  alternative_hypothesis = TRUE, hit = TRUE, miss = TRUE,
  show_sigma = TRUE,
  fill = TRUE,
  decision = TRUE, 
  AX = TRUE, 
  AZ = FALSE, 
  data_points = NULL, 
  arrow = FALSE,
  area = NULL
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
  d <- (m1 - m0) / sigma
  ### X and Ys
  X0s <- seq(m0 - sigma_range * sigma_m, m0 + sigma_range * sigma_m, by = 0.01 * sigma_m)
  X1s <- seq(m1 - sigma_range * sigma_m, m1 + sigma_range * sigma_m, by = 0.01 * sigma_m)
  Xs <- sort(cbind(X0s, X1s))
  if (is.null(x_range)) Xs <- Xs else Xs <- seq(min(x_range), max(x_range), by = 0.01 * sd(x_range))
  Y0s <- dnorm(Xs, mean = m0, sd = sigma_m)
  Y1s <- dnorm(Xs, mean = m1, sd = sigma_m)
  
  #### Calculate values for Null Hypothesis
  if (two_tails) {
  X_min <- qnorm(alpha_level / 2, mean = m0, sd = sigma_m, lower.tail = TRUE)
  X_max <- qnorm(alpha_level / 2, mean = m0, sd = sigma_m, lower.tail = FALSE)
  low_tail <- seq(min(min(Xs), X_min), max(min(Xs), X_min), by = sigma_m * 0.001)
  high_tail <- seq (X_max, max(Xs), by = sigma_m * 0.001)
  alpha_value <- sub("^0\\.", ".", formatC(alpha_level / 2, digits = 2, format = "fg"))
  correct_rject_value <- sub("^0\\.", ".", formatC(1 - alpha_level, digits = 2, format = "fg"))
  boddy <- seq(X_min, X_max, by = sigma_m * 0.001)
  tail_x <- c(min(low_tail), low_tail, max(low_tail), min(high_tail), high_tail, max(high_tail) )
  tail_y <- c(0, dnorm(low_tail,  mean = m0, sd = sigma_m), 0, 
              0, dnorm(high_tail, mean = m0, sd = sigma_m), 0)  	
  } else {
  X_max <- qnorm(alpha_level, mean = m0, sd = sigma_m, lower.tail = FALSE)
  high_tail <- seq (X_max, max(Xs), by = sigma_m * 0.001)
  alpha_value <- sub("^0\\.", ".", formatC(alpha_level, digits = 2, format = "fg"))
  correct_rject_value <- sub("^0\\.", ".", formatC(1 - alpha_level, digits = 2, format = "fg"))
  boddy <- seq(min(Xs), X_max, by = sigma_m * 0.001)
  tail_x <- c(min(high_tail), high_tail, max(high_tail) )
  tail_y <- c(0, dnorm(high_tail, mean = m0, sd = sigma_m), 0)  	
  }

  tail_col <- col2alpha(plot_colors[1], alpha = 0.5)
  body_x <- c(min(boddy), boddy, max(boddy))
  body_y <- c(0, dnorm(boddy, mean = m0, sd = sigma_m), 0)
  body_col <- col2alpha(plot_colors[2], alpha = 0.5)
   
  #### Calculate values for Alternative Hypothesis
  beta_value <- pnorm(q = high_tail, mean = m1, sd = sigma_m)
  beta_value <- formatC(beta_value, digits = 2, format = "fg")
  beta_value <- sub("^0\\.", ".", beta_value)
  power_value <- pnorm(q = high_tail, mean = m1, sd = sigma_m, lower.tail = FALSE)
  power_value <- formatC(power_value, digits = 2, format = "f")
  power_value <- sub("^0\\.", ".", power_value)
  beta_range <- seq(min(min(X1s), X_max), max(min(X1s), X_max), by = sigma_m * 0.001)
  beta_x <- c(min(X1s), beta_range, X_max)
  beta_y <- c(0, dnorm(beta_range, mean = m1, sd = sigma_m), 0)
  beta_col <- col2alpha(plot_colors[3], alpha = 0.5)
  power_range <- seq( X_max, max(X1s), by = sigma_m * 0.001)
  power_x <- c(X_max, power_range, max(X1s))
  power_y <- c(0, dnorm(power_range, mean = m1, sd = sigma_m), 0)
  power_col <- col2alpha(plot_colors[4], alpha = 0.5)
  
  label_pos <- seq(min(Xs), max(Xs), by = sigma_m)
  label_pos <- unique(c(label_pos, data_points))

 ######## Do the plot
  plot(NULL, NULL, 
    xlim = range(Xs),
    ylim = c(0, (1 + 0.01) * max(Y0s) ),
    axes = FALSE, xlab = "", ylab = "", yaxs = "i")
  
 ####### Plot the null hypothesis
  if (null_hypothesis){
  	if (false_alarm & fill) {
      polygon(tail_x, tail_y, col = tail_col, border = "white")
      if (show_alpha_level){
      	if (two_tails){
          text(x = X_min, y = dnorm(m0, m0, sd = sigma_m) / 15, 
           label = bquote(alpha / 2 == .(alpha_value)), pos = 2)      		
      	  } else {
          text(x = X_max, y = dnorm(m0, m0, sd = sigma_m) / 15, 
           label = bquote(alpha == .(alpha_value)), pos = 4)       	  	
      	  }
      }
   }
   if (correct_reject & fill) {
      polygon(body_x, body_y, col = body_col, border = "white")
      if (show_alpha_level){
      text(x = m0 - sigma_m * 0.6, y = dnorm(m0, m0, sd = sigma_m) / 3,
           bquote(1 - alpha == .(correct_rject_value)), pos = 3)
      }
    }
    lines(Xs, Y0s, col = "#cf232a", lwd = 2)
    if (show_sigma){
    abline(v = m0, lwd = 1)
    arrows(x0 = m0, y0 = dnorm(m0 + sigma_m, m0, sigma_m),
           x1 = m0 + sigma_m, y1 = dnorm(m0 + sigma_m, m0, sigma_m),
           length = 0.1)
    if (AX | (!AX & ! AZ)) sigma_mm <- sigma_m else sigma_mm <- 1
    text(x = m0 + sigma_m / 2, y = dnorm(m0 + sigma_m, m0, sigma_m), pos = 1,
         labels = if (n==1) bquote(sigma == .(round(sigma_mm, 2)))
                       else bquote(sigma[M] == .(round(sigma_mm, 2)))
         )
  mtext(expression(H[0]), side = 3, at = m0)    	
    }
  }
  
  ###### Plot the alternative hypothesis
  if (alternative_hypothesis){
    if (miss & fill) {
    polygon(beta_x, beta_y, col = beta_col, border = "white")
    if (show_alpha_level){
      text(x = X_max, y = dnorm(m1, m1, sd = sigma_m) / 15,
           labels = bquote(beta == .(beta_value)), pos = 2)
      }
    }
  if (hit & fill) {
    polygon(power_x, power_y, col = power_col, border = "white")
    if (show_alpha_level){
      text(x = X_max + abs(m1 - X_max) * 0.5,
           y = dnorm(m1, m1, sd = sigma_m) / 3,
           labels = bquote(1-beta == .(power_value)), pos = 4)
      }
    }
  lines(Xs, Y1s, col = "#d5493a", lwd = 2)
  if (show_sigma) {
    abline(v = m1, lwd = 1, col = "gray")
    mtext(expression(H[1]),  side = 3, at = m1)
    mtext(text = bquote("Cohen's d" == .(d)~", n"==.(n)), side = 3, at = (m0 + m1) / 2, padj = -2)	
   }
  }

 ####### X-Axes and Z-Axes
  AXZ <- function (Baseline = 2.5, Axis = "z", show_label = TRUE) {
  	llabel <- if (Axis == "z") {round((label_pos - m0) / sigma_m, 2)} else {round(label_pos, 2)}
    axis(1, at = c(min(label_pos) - 0.5 * sigma_m, max(label_pos) + 0.5 * sigma_m), labels = c("", ""), lwd.ticks = 0, line = Baseline)
    if (show_label) {
    axis(1, at = label_pos, label = rep("", length(label_pos)), lwd = 0, lwd.ticks = 1, line = Baseline)
    axis(1, at = label_pos, label = llabel, lwd = 0, lwd.ticks = 0, line = Baseline - 0.5)
    mtext(text = bquote(italic(.(Axis))), side = 1, adj = 1.03, line = Baseline - 0.6)	
    }
  }

  if (AX) {
  	if (n == 1) AXZ(  0, "X") else AXZ(0, "M")
    if (AZ)     AXZ(2.5, "z")
  } else {
  	if (AZ) AXZ(0, "z") else AXZ(0, "z", show_label = FALSE)
  }

  
 ####### Add an area
 if (!is.null(area)){
  p1 <- area[1]
  p2 <- area[2]
  cnvtp <- function(pp) if (pp == Inf) m0 + sigma_range * sigma_m else if (pp == -Inf) m0 - sigma_range * sigma_m  else pp
  if (! is.na(p1) & ! is.na(p2)){
    pt1 <- cnvtp(p1)
    pt2 <- cnvtp(p2)
    polygon(c(pt1, pt1,                        seq(pt1, pt2, length.out = 1000)          ,        pt2,           pt2),
            c(0,  dnorm(pt1, m0, sigma_m),  dnorm(seq(pt1, pt2, length.out = 1000), m0, sigma_m), dnorm(pt2, m0, sigma_m), 0),
            col = col2alpha("#f4cdba", alpha = 0.8) , border = "#af1e23", lwd = 2) ## col2alpha is defined in pPower.R
    percent <- function(x, digits = 2, format = "f", ...) {
      paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
    }
    area_size <- if(p1 == -Inf) pnorm(p2, m0, sigma_m)
                   else if (p2 == +Inf) pnorm(p1, m0, sigma_m, lower.tail = FALSE)
                else pnorm(p2, m0, sigma_m) - pnorm(p1, m0, sigma_m)
    area_size <- percent(area_size, 2)
    text(x = mean(c(pt1, pt2)), y = dnorm(m0 + 2 * sigma_m, m0, sigma_m), label = area_size, pos = 1)
  }	
 }

  ###### add some data points
  if (!is.null(data_points)) {
    for (point in data_points) {
       mtext(bquote(X == ~.(point)), side = 1, at = point, line = if (AZ) 4 else 2)
       arrows(point, -dnorm(m0 + sigma_m, m0, sigma_m) / (if (AZ) 3.8 else 8.5), point, 0, xpd = TRUE, length = 0.08)
     }
  }

 ####### show the decision
  if(decision){
    abline(v = X_max, lty = 3)
    text(x = X_max, y = dnorm(m0, m0, sd = sigma_m) / 1.1,
          label = expression(paste("Reject ", H[0])),  pos = 4)
    arrows(x0 = X_max, y0 = dnorm(m0, m0, sd = sigma_m) / 1.16,
           x1 = X_max + sigma_m, length = 0.1)
    if (two_tails) {
    abline(v = X_min, lty = 3)
    text(x = X_min, y = dnorm(m0, m0, sd = sigma_m) / 1.1,
         label = expression(paste("Reject ", H[0])), pos = 2)
    arrows(x0 = X_min, y0 = dnorm(m0, m0, sd = sigma_m) / 1.16,
           x1 = X_min - sigma_m, length = 0.1)    	
    }
  }
  
  ####### Add the arrow showing the standard errors
  if (arrow) {
    segments(
    x0 = seq(m0 - 2 * sigma_m, m0  + 2 * sigma_m, by = sigma_m), y0 = 0,
    y1 = c(rep(dnorm(m0 + sigma_m, m0, sigma_m), 2), dnorm(m0, m0, sigma_m), 
      rep(dnorm(m0 + sigma_m, m0, sigma_m), 2)), lty = 5)
    arrows(
    x0 = c(m0, m0, m0 + sigma_m, m0 - sigma_m), y0 = dnorm(m0 + sigma_m, m0, sigma_m),
    x1 = c(m0 + sigma_m, m0 - sigma_m, m0 + 2 * sigma_m, m0 - 2 * sigma_m), length = 0.08)
   }
   
}  