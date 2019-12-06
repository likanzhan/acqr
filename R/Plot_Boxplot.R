#' Plot a boxplot
#'
#' Illustrate how a boxplot and five number summary are calculated
#'
#' @param data a numeric vector
#' @examples
#' fat <- c(PASWR::Bodyfat[["fat"]], 60)
#' Plot_Boxplot(fat)
#' @export

Plot_Boxplot <- function(data) {
  fvnm <- fivenum(data)
  H_spread <- fvnm[4] - fvnm[2]
  IFL <- fvnm[2] - 1.5 * H_spread
  IFU <- fvnm[4] + 1.5 * H_spread
  OFL <- fvnm[2] - 3.0 * H_spread
  OFU <- fvnm[4] + 3.0 * H_spread
  YMN <- OFL - 0.5 * H_spread
  YMX <- OFU + 0.5 * H_spread

  svnm <- c(fvnm, IFL, IFU, OFL, OFU)
  names <- c(
    "Min",
    expression("H"[L]),
    expression("Q"[2]),
    expression("H"[U]),
    "Max",
    expression("IF"[L]),
    expression("IF"[U]),
    expression("OF"[L]),
    expression("OF"[U])
  )
  bp <- boxplot(data,
    horizontal = TRUE,
    ylim = c(min(YMN, min(data)), max(YMX, max(data))),
    col = "springgreen3", notch = TRUE, axes = FALSE, outpch = NA
  )
  with(bp, points(out, group, pch = ifelse(out >= OFU | out <= OFL, 16, 1)))
  box()
  text(x = svnm, y = rep(1.5, length(svnm)), label = names, pos = 1)
  points(x = data, y = jitter(rep(1.3, length(data))), pch = 16)
  invisible(sapply(
    c(2, 4, 6, 7, 8, 9),
    function(x) lines(x = rep(svnm[x], 2), y = c(0.6, 1.4), lty = 2)
  ))
  invisible(sapply(
    c(1, 5),
    function(x) lines(x = rep(svnm[x], 2), y = c(1, 1.4), lty = 2)
  ))
  invisible(sapply(
    list(c(2, 4), c(6, 2), c(4, 7), c(6, 8), c(7, 9)),
    function(x) {
      arrows(
        x0 = (svnm[x][1]) + 0.02 * H_spread,
        x1 = (svnm[x][2]) - 0.02 * H_spread, y0 = 0.7, length = 0.08, code = 3
      )
    }
  ))
  arrows(x0 = OFL - 0.02 * H_spread, x1 = YMN, y0 = 0.7, length = 0.08, code = 2)
  arrows(x0 = OFL + 0.02 * H_spread, x1 = YMX, y0 = 0.7, length = 0.08, code = 2)
  text(x = mean(svnm[c(2, 4)]), y = 0.7, label = expression(italic("H"[spread])), pos = 1)
  text(x = mean(svnm[c(4, 7)]), y = 0.7, label = expression(italic("1.5 * H"[spread])), pos = 1)
  text(x = mean(svnm[c(2, 6)]), y = 0.7, label = expression(italic("1.5 * H"[spread])), pos = 1)
  text(x = mean(svnm[c(6, 8)]), y = 0.7, label = expression(italic("1.5 * H"[spread])), pos = 1)
  text(x = mean(svnm[c(7, 9)]), y = 0.7, label = expression(italic("1.5 * H"[spread])), pos = 1)
  text(x = mean(svnm[c(1, 6)]), y = 0.9, label = "Outliers", pos = 1)
}
