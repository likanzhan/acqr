LatinSqr <- function(n = 5, Latin = TRUE){

x <- matrix(1:n, n, n, byrow = TRUE)
colnames(x) <- LETTERS[1:n]

if (Latin) {
x[1, c(1, 1 : floor(n / 2) * 2)]  <- 1 : (floor(n / 2) + 1)
x[1, (2 : ceiling(n / 2)) * 2 - 1 ] <- n + 2  - (2: ceiling(n / 2))
}

for (i in 2:n)  x[i, ] <- x[i-1, c(2:n, 1)]
return(x)	
}
