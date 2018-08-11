Calculate_Kernel_Density <- function(
  x, h = 1,
  x0 = seq(min(x), max(x), length = length(x)),
  kernel = c("rectangle", "triangle", "gauss")
  ) {
  # Define the kernel
  if (length(kernel) > 1) kernel <- "rectangle" else kernel <- kernel
  # rectangle <- function(z) ifelse(abs(z) < 1, 0.5, 0)
  # triangle <- function(z) ifelse(abs(z) < 1, 1 - abs(z), 0)
  # gauss <- function(z) dnorm(z)
  rectangle <- function(z) (abs(z) < 1) * 0.5
  triangle <- function(z) (abs(z) < 1) * (1 - abs(z))
  gauss <- function(z) 1 / sqrt(2 * pi) * exp( - (z ^ 2) / 2)
  kernel <- eval(parse(text = kernel))

  # Calculate the density
  n <- length(x)
  fun <- function(i) {
    x0 <- rep(x0[i], n)
    xz <- (x - x0) / h
    xzk <- kernel(xz)
    xzk1 <- (1 / (n * h)) * xzk
    xzks0 <- sum(xzk1)
    xzks <- rep(xzks0, n)
    data.frame(x = x, x0 = x0, xz = xz, xzk = xzk, xzk1 = xzk1, xzks = xzks)
  }
  ll <- lapply(1:length(x0), fun)
  do.call(rbind, ll)
}