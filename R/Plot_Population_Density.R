Plot_Population_Density <- function(
mean = 100, sd = 15, point = NULL,
X = seq((mean - 3 * sd),  (mean + 3 * sd), by = 0.01),
AZ = FALSE, AX = FALSE, arrow = FALSE, fill = TRUE
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
axis(1, at = c(min(X) - 1.5 * sd, max(X) + 1.5 * sd), labels = c("", ""), line = 2.5, lwd.ticks = 0)
axis(
  1, at = seq(min(X) - 1 * sd, max(X) + 1 * sd, by = sd),
  labels = seq(min(X) - 1 * sd, max(X) + 1 * sd, by = sd) / sd, line = 2.5, lwd.ticks = 1)
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

if(fill){
  pt1 <- min(X)
  pt2 <- max(X)
  polygon(c(pt1, pt1,                        seq(pt1, pt2, length.out = 1000)          ,        pt2,           pt2),
          c(0,  dnorm(pt1, mean, sd), dnorm(seq(pt1, pt2, length.out = 1000), mean, sd), dnorm(pt2, mean, sd), 0),
          col = col2alpha("#f4cdba", alpha = 0.8) , border = "#af1e23", lwd = 2) ## col2alpha is defined in pPower.R
}

}
