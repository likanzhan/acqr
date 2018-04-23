Density_Plot <- function(
mean, sd, point = NULL,
X = seq((mean - 3 * sd),  (mean + 3 * sd), by = 0.01),
AZ = FALSE, AX = FALSE, arrow = FALSE
){
plot(
  X, dnorm(X, mean, sd), xlim = c(min(X), max(X)),
  type = "l", xlab = "", ylab = "", axes = FALSE, col = "#af1e23", lwd = 2, yaxs = "i")
text(x= mean, y = 0, label = bquote(mu==~.(mean)), adj = c(-0.1, -0.7))
axis(1, at = c(min(X) - 1.5 * sd, max(X) + 1.5 * sd), labels = c("", ""), line = 0, lwd.ticks = 0)
if (AX) {
axis(1, at = seq(min(X) - 1 * sd, max(X) + 1 * sd, by = sd), lwd = 0, line = 0, lwd.ticks = 1)	
}
mtext(expression(italic("X")), side = 1, line = -0.5, adj = 1.02)
if (AZ) {
axis(1, at = c(mean - 3.5 * sd, mean + 3.5 * sd), labels = c("", ""), line = 2.5, lwd.ticks = 0)
axis(
  1, at = seq(mean - 3 * sd, mean + 3 * sd, by = sd), 
  labels = seq(-3, 3, by = 1), line = 2.5, lwd.ticks = 1)
mtext(expression(italic("z")), side = 1, line = 2, adj = 1.02)	
}
if (arrow) {
segments(
  x0 = seq(mean - 2 * sd, mean + 2 * sd, by = sd), y0 = 0,
  y1 = c(rep(dnorm(mean + sd, mean, sd), 2), dnorm(mean, mean, sd), rep(dnorm(mean + sd, mean, sd), 2)),
  lty = 5)
text(
  x = mean + sd / 2, y = dnorm(mean + sd, mean, sd),
  pos = 3, label = bquote(sigma == ~.(sd)))
arrows(
  x0 = c(mean, mean, mean + sd, mean - sd), y0 = dnorm(mean + sd, mean, sd),
  x1 = c(mean + sd, mean - sd, mean + 2 * sd, mean - 2 * sd), length = 0.08)	
}
if (!is.null(point)) {
for (point in point) {
mtext(bquote(X == ~.(point)), side = 1, at = point, line = 2)
arrows(point, -dnorm(mean + sd, mean, sd) / 6, point, 0, xpd = TRUE, length = 0.08)		
}
}
}