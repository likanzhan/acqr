Plot_t_Distribution <- function (
n = 3,
alpha = 0.05,
two_tails = TRUE,
critical = FALSE
){
if (two_tails) {
 z_value <- qnorm(p = alpha / 2, lower.tail = FALSE)	
 t_value <- qt(p = alpha / 2, df = n - 1, lower.tail = FALSE)	
} else {
 z_value <- qnorm(p = alpha, lower.tail = FALSE)	
 t_value <- qt(p = alpha, df = n - 1, lower.tail = FALSE)		
}
	
xx <- seq(-6, 6, by = 0.01)
zy <- dnorm(xx)
ty <- dt(xx, n - 1)
maxy <- max(c(zy, ty))
zcolor <- ggsci::pal_aaas("default")(10)[2]
tcolor <- ggsci::pal_aaas("default")(10)[3]
plot(x = xx, y = zy, ylim = c(0, 1.2 * maxy), 
     col = zcolor, type = "l",
     xlab = "z/t value", ylab = "density",
     bty = "l", yaxs = "i"
 )
 lines(xx, ty, col = ggsci::pal_aaas("default")(10)[3])
 text( x = 0, y = maxy / 1.75, labels = bquote(n == .(n)), col = tcolor)
 text( x = 0, y = maxy / 2, labels = bquote(df == .(n - 1)), col = tcolor)
 legend("top", lty = c(1, 1),  col = c(zcolor, tcolor), bty = "n",
   legend = c("unit normal distribution", "t distribution")
 )
 if (critical) {
  if (two_tails){
    abline(v = c(z_value, -z_value), col = zcolor, lty = 2)
    abline(v = c(t_value, -t_value), col = tcolor, lty = 2)	
  } else {
    abline(v = z_value, col = zcolor, lty = 2)
    abline(v = t_value, col = tcolor, lty = 2)	  	
  }
  text(x = 0, y = maxy / 2.5,
    labels = bquote(italic(alpha / 2) == .(alpha / 2)))
  zvalue <- formatC(z_value, digits = 6, format ="f")
  text(x = z_value,y = maxy / 4, adj = - 0.05, col = zcolor,
    labels = bquote(italic(z) == .(zvalue)))
  tvalue <- formatC(t_value, digits = 6, format ="f")
  text(x = t_value, y = maxy / 1, adj = - 0.05, col = tcolor,
    labels = bquote(italic(t) == .(tvalue))
    )	
  }
}