#' Anscombe
#' @export

Anscombe <- function(model, q = 0.05) {
  k <- model[["rank"]]
  n <- k + model[["df.residual"]]
  pz <- q * (n - k - 1) / n
  qz <- qnorm(pz, lower.tail = FALSE)
  m <- 1.4 + 0.85 * qz
  Eqt <- m * (1 - ((m^2 - 2) /
    (4 * (n - k - 1)))) *
    sqrt((n - k - 1) / n)
  Eqs <- Eqt * sqrt((n - k - 2) /
    (n - k - 1 - Eqt^2))
  Emax <- max(abs(rstudent(model)))
  results <- data.frame(
    Pre = q,
    ZPro = pz,
    ZQnt = qz,
    MVal = m,
    ECut = Eqt,
    EStud = Eqs,
    EObsMax = Emax
  )
  return(results)
}
