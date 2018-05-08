Density_Plot_with_Pologon <- function (p1 = mean, p2 = mean + sd, mean = 100, sd = 15, AX = TRUE, AZ = TRUE, show_area_size = TRUE)
{
  points <- c(p1, p2)
  points <- points[is.finite(points)]
  acqr::pPower(m0 = mean, x_range = 4, m1 = mean, sigma = sd, H1 = F, reject = F, level = F, beta = F, power = F, body = F, tail = F, AX = AX, AZ = AZ, points = points)
  cnvtp <- function(pp) if (pp == Inf) mean + 4 * sd else if (pp == -Inf) mean - 4 * sd else pp
  if (! is.na(p1) & ! is.na(p2)){
    pt1 <- cnvtp(p1)
    pt2 <- cnvtp(p2)
    polygon(c(pt1, pt1,                        seq(pt1, pt2, length.out = 1000)          ,        pt2,           pt2),
            c(0,  dnorm(pt1, mean, sd), dnorm(seq(pt1, pt2, length.out = 1000), mean, sd), dnorm(pt2, mean, sd), 0),
            col = col2alpha("#f4cdba", alpha = 0.8) , border = "#af1e23", lwd = 2) ## col2alpha is defined in pPower.R
    percent <- function(x, digits = 2, format = "f", ...) {
      paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
    }
    area_size <- if(p1 == -Inf) pnorm(p2, mean, sd)
                   else if (p2 == +Inf) pnorm(p1, mean, sd, lower.tail = FALSE)
                else pnorm(p2, mean, sd) - pnorm(p1, mean, sd)
    area_size <- percent(area_size, 2)
    if (show_area_size) {
      text(x = mean(c(pt1, pt2)), y = dnorm(mean + 2 * sd, mean, sd), label = area_size, pos = 1)
    }
  }
}
