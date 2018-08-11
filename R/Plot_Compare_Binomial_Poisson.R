Plot_Compare_Binomial_Poisson <- function(
pi = 0.1,
size = 100
){
xx <- 0:size
db <- rep(xx, round(dbinom(xx, size, pi) * 100000, 0))
db <- ecdf(db)
dbb <- db(xx)
lambda <- size * pi
dp <- rep(xx, round(dpois(xx, lambda) * 100000, 0))
dp <- ecdf(dp)
dpp <- dp(xx)
plot(db, col = "blue", cex.points = 1 / log10(size),
  xlab = "X", ylab = "Cumulative Distribution Function", 
  main = bquote(pi == .(pi)~", size" == .(size)))
plot(dp, add = TRUE, col = "red", cex.points = 1 / log10(size))
legend("bottomright", col = c("blue", "red"), lty = 1, bty = "n",
  legend = c("Binomial Distribution", "Poisson Distribution"))
}