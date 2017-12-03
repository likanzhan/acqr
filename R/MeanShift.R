MeanShift <- function(formula, data) {
 if (class(formula) == "lm") {
  fmO <- formula
  data <- formula[["model"]]
 } else {	
  fmO <- lm(formula, data = data)
 }
MeanShiftI <- function (i) {
data[, "D"] <- replace(rep(0, nrow(data)), i, 1)
fmN <- update(fmO, .~. + D, data = data)
fmNS <- summary(fmN)
res <- coefficients(fmNS)["D", c(3:4)]
#res["Bonfer.p", ] <-  2 * nrow(data) * res[2, ] / 2
return(res)
}	
  MeanShift <- sapply(1:nrow(data), FUN = MeanShiftI)
  MeanShift <- as.data.frame(t(MeanShift))
  MeanShift[, 3] <- 2 * nrow(data) * MeanShift[, 2] / 2
  colnames(MeanShift) <- c("Student.Res", "p.values", "Bonfer.p")	
  return(MeanShift)
}