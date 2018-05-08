LatinSqr <- function(n = 5, Latin = TRUE){

x <- matrix(1:n, n, n, byrow = TRUE)
colnames(x) <- if (n <= 26) LETTERS[1:n] else sprintf(paste("C%0", max(nchar(n)), "d", sep=""), 1:n)

if (Latin) {
x[1, c(1, 1 : floor(n / 2) * 2)]  <- 1 : (floor(n / 2) + 1)
x[1, (2 : ceiling(n / 2)) * 2 - 1 ] <- n + 2  - (2: ceiling(n / 2))
}
for (i in 2:n)  x[i, ] <- ifelse(x[i-1, ] + 1 <= n, x[i-1, ] + 1, x[i-1, ] + 1 - n)
return(x)
}
