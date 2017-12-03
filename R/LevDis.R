LevDis <- function(
  n = 7, x0 = mean(1:n), y0 = -1, slope = 1,
  title = "a). A low leverage outlier"
){
x <- 1:n
y <- slope * x
xn <- c(x, x0)
yn <- c(y, y0)
plot(y ~ x, 
     xlim = c(min(xn) - 1, max(xn) + 1), 
     ylim = c(min(yn) - 1, max(yn) + 1),
     xaxt ="n", yaxt = "n", xlab = "", ylab = "" )
title(ylab = "Y", line = 0.2, adj = 1, srt = 90)
title(xlab = "X", line = 0.2, adj = 1)
points(x0, y0, pch = 19, col = "red")
abline(lm(y ~ x))
abline(lm(yn ~ xn), lty = 2)
mtext(title, side = 3, adj =1 / 2, line = 0.1)
}