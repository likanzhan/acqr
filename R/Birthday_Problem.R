# https://en.wikipedia.org/wiki/Birthday_problem

Birthday_Problem <- function(size, days = 366) {
  if (!suppressMessages(require("Brobdingnag"))) stop("The package `Brobdingnag` was not installed")
  BP <- function(n, year) {
    if (n >= year) {
      BP <- 1
    } else {
      birthday <- Brobdingnag::prod(Brobdingnag::as.brob(year:1)) / (Brobdingnag::as.brob(year)^n * Brobdingnag::prod(as.brob((year - n):1)))
      birthday <- as.numeric(abs(birthday))
      BP <- 1 - birthday
    }
    return(BP)
  }
  sapply(size, FUN = function(x) BP(x, days))
}
