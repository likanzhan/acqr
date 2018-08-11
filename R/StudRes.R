StudRes <- function(formula, data) {
  if (class(formula) == "lm") {
    fm <- formula
    data <- formula[["model"]]
  } else {
    fm <- lm(formula, data = data)
  }
  n <- nrow(model.matrix(fm))
  k <- ncol(model.matrix(fm)) - 1
  SRI <- function(i) {
    Rei <- residuals(fm)[i]
    SEi <- sqrt(deviance(update(fm, data = data[-i, ])) / (n - k - 2))
    Hi <- hatvalues(fm)[i]
    StuResi <- Rei / (SEi * sqrt(1 - Hi))
    return(StuResi)
  }
  StuRes <- sapply(1:n, FUN = SRI)
  p.val <- 2 * pt(abs(StuRes), n - k - 2, lower.tail = FALSE)
  adjusted.p <- 1 - (1 - p.val)^n
  Bonfer.p <- 2 * n * p.val / 2
  StudRes <- data.frame(
    Student.Res = StuRes,
    p.values = p.val,
    Bonfer.p = Bonfer.p
  )
  return(StudRes)
}
