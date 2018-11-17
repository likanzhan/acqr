#' @export

Create_Correlation_Data <- function(rr = 1, number = 50, nrm = TRUE){
if (nrm) {
  set.seed(2)
  df <- MASS::mvrnorm(
    n = number, mu = c(0, 0), 
    Sigma = matrix(c(1, rr, rr, 1), ncol = 2), 
    empirical = TRUE)
    df <- as.data.frame(df)
    colnames(df) <- c("X", "Y")	
  } else {	
  X <- seq(-3, 3, length.out = number)             
  yfun <- function(x = X, a = 1, h = 0, k = 1)  a * (x - h) ^ 2 + k
  Y <- yfun(X)
  df <- data.frame(X = X, Y = Y)	
 }
return(df)
}

Plot_Data_Correlation <- function(rr = 1, number = 50, nrm = TRUE){
df <- Create_Correlation_Data(rr = rr, number = number, nrm = nrm)
rr <- cor(df[, "X"], df[, "Y"])
rr <- round(rr, 3)
plot(Y ~ X, data = df, pch = 19, col = "#cb2229",
  bty = "l", xaxt = 'n', yaxt = 'n',
  xlab = "", ylab = "", font.lab = 2)
title(xlab = "X", ylab = "Y", line = 0)  
text(
  x = mean(range(df[, "X"])),
  y = max(df[, "Y"]),
  labels = bquote(r == .(rr))
  )  
}