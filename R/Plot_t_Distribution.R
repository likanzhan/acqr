Plot_t_Distribution <- function (
n = 3,
alpha_level = 0.05,
sigma_range = 6,
two_tails = TRUE,
show_critical_region = FALSE, 
fill_critical_region = FALSE, 
fill_confidence_interval = FALSE,
show_normal_curve = TRUE,
data_points = NULL
){
 ##### 1. Colors used in the plotting
 plot_colors <- c("#008744", "#0057e7", "#d62d20", "#ffa700") # google

 zcolor <- "#008744" #ggsci::pal_aaas("default")(10)[2]
 tcolor <- "#d62d20" #ggsci::pal_aaas("default")(10)[3]

 ##### 2. Calculate the relevant values for the plotting
 
 x_value <- seq(- sigma_range, sigma_range, by = 0.01) 
 alpha_value <- ifelse(two_tails, alpha_level / 2, alpha_level)
 
 t_value <- qt(p = alpha_value, df = n - 1, lower.tail = FALSE)
 z_value <- qnorm(p = alpha_value, lower.tail = FALSE)
 tvalue <- formatC(t_value, digits = 6, format = "f")
 zvalue <- formatC(z_value, digits = 6, format = "f")

 y_value_z <- dnorm(x_value)
 y_value_t <- dt(x_value, df = n - 1)
 max_y <- max(y_value_z)

 # calculate critical region #
 polygon_right_x <- c(t_value,   x_value[x_value >= t_value],              max(x_value))
 polygon_right_y <- c(0,      dt(x_value[x_value >= t_value], df = n - 1), 0           )
 
 # calculate confidence interval
 polygon_between_x <- c(-t_value,    x_value[x_value >= - t_value & x_value <= t_value],             t_value)
 polygon_between_y <- c(0       , dt(x_value[x_value >= - t_value & x_value <= t_value], df = n-1),  0      )


 ##### 3. Do the plotting 
 plot(x = NULL, y = NULL, type = "l", bty = "l", yaxs = "i", axes = FALSE,
     xlim = range(x_value), ylim = c(0, 1.2 * max(y_value_z)),  
     xlab = "z / t value", ylab = "Density", font.lab = 2)
 
 if (fill_critical_region) {
 	if (two_tails) {
  polygon(x = - polygon_right_x, y = polygon_right_y, border = "white", col = "#f4cdba") }
  polygon(x = polygon_right_x, y = polygon_right_y, border = "white", col = "#f4cdba")     
 }
 
 if (fill_confidence_interval){
  polygon(x = polygon_between_x, y = polygon_between_y, border = "white", col = "#f4cdba")
  segments(- t_value, 0, - t_value, dt(0, df = n - 1) / 1.5)   	
  segments(+ t_value, 0, + t_value, dt(0, df = n - 1) / 1.5)
  text(x = 0, y = dt(0, df = n - 1) / 10, pos = 3, 
   label = bquote("Middle"~.(100 - alpha_level * 100)~"% of"~italic(t)~"distribution") )	
 }

 lines(x_value, y_value_t, col = tcolor, lwd = 2)
 text(x = 0, y = max_y / 1.75, labels = bquote(n == .(n)), col = tcolor)
 text(x = 0, y = max_y / 2, labels = bquote(df == .(n - 1)), col = tcolor)
 
 axis(2, at = c(0, max_y + 0.2), labels = c("", ""), lwd.ticks = 0)
 axis(2, at = seq(0, max_y + 0.1, by = 0.1), lwd = 0, lwd.ticks = 1)
 axis(1, at = c(min(x_value) - 0.5, max(x_value) + 0.5), labels = c("", ""), lwd.ticks = 0)
 axis(1, at = seq(min(x_value), max(x_value), by = 1), lwd = 0, lwd.ticks = 1)

 if (show_normal_curve) {
   legend("top", lty = c(1, 1),  col = c(zcolor, tcolor), bty = "n",
     legend = c("unit normal distribution", "t distribution"))	
   } else {
   legend("top", lty = 1,  col = tcolor, bty = "n", legend = "t distribution") 	
  }
  
 if (show_critical_region) {
  if (two_tails) abline(v = c(t_value, -t_value), col = tcolor, lty = 2) 
            else abline(v = t_value, col = tcolor, lty = 2)
  text(x = t_value, y = max_y / 1, adj = - 0.05, col = tcolor,
    labels = bquote(italic(t) == .(tvalue)))
  text(x = 0, y = max_y / 2.5, labels = bquote(italic(alpha / 2) == .(alpha_value)))	
  }

 ##### 4. Add normal curve 
 if (show_normal_curve) {
  lines(x_value, y_value_z, col = zcolor, lwd = 2) 
  if (show_critical_region) {
  if (two_tails) { abline(v = c(z_value, -z_value), col = zcolor, lty = 2) }
            else { abline(v = z_value, col = zcolor, lty = 2) }  	
  text(x = z_value,y = max_y / 4, adj = - 0.05, col = zcolor,
    labels = bquote(italic(z) == .(zvalue))) 
   }
  }
  
  ###### add some data points
  if (!is.null(data_points)) {
    for (point in data_points) {
       mtext(bquote(t == ~.(point)), side = 1, at = point, line = 2)
       arrows(point, -dt(0, n-1) / 7, point, 0, xpd = TRUE, length = 0.08)
     }
  }
}

###
# Plot_t_Distribution(n=9, show_critical_region=T, fill_critical_region=T, show_normal_curve=F, data_points= 2.5)