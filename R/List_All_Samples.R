Sampling_Grid <- function (data, size = NA, fun = NA, bp = 0.5){
 if (length(data) == 1) {
   ln <- list(0:1)
   size <- data
   Mean <- size * bp
   SD <- sqrt(size * bp * (1 - bp))
   if (is.na(fun)) fun <- "sum" else fun <- fun
   }
 else {
   ln <- list(data)
   size <- ifelse(is.na(size), length(data), size)
   Mean <- mean(data)
   SD <- sd(data)
   if (is.na(fun)) fun <- "mean" else fun <- fun
 }
rn <- rep(ln, size)
Sampled_Grid <- expand.grid(rn)
colnames(Sampled_Grid) <- sprintf(paste("S%0", max(nchar(size)), "d", sep = ""), 1:size)
Sampled_Grid["Statistic"] <- apply(Sampled_Grid, 1, fun)
Res <- list("Sampled_Grid" = Sampled_Grid, "Mean" = Mean, "SD" = SD)
return(Res)
}

# x <- c(2, 4, 6, 8, 11, 15, 50, 60)
# xx <- Sampling_Grid(x, size = 3)[["Sampled_Grid"]][["Statistic"]]

