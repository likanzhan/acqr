show_boxplot <- function(data){
fvnm <- fivenum(data)
H_spread <- fvnm[4] - fvnm[2]
FL <- fvnm[2] - 1.5 * H_spread
FU <- fvnm[4] + 1.5 * H_spread
svnm <- c(fvnm, FL, FU)
names <- c(
  "Min", expression("H"[L]), expression("Q"[2]), expression("H"[U]), "Max", 
  expression("F"[L]), expression("F"[U]))
boxplot(fat, horizontal = T, ylim = range(svnm), col = "springgreen3", notch = TRUE)
text(x = svnm, y = rep(1.5, length(svnm)), label = names, pos = 1)
points(x = fat, y = jitter(rep(1.3, length(fat))), pch = 16)
invisible(sapply(
  c(2, 4, 6, 7), 
  function(x) lines(x = rep(svnm[x], 2), y = c(0.6, 1.4), lty = 2)
  ))
invisible(sapply(
  c(1, 5), 
  function(x) lines(x = rep(svnm[x], 2), y = c(1, 1.4), lty = 2)
  ))
invisible(sapply(
  list(c(2, 4), c(6, 2), c(4, 7)),
  function(x) arrows(x0 = (svnm[x][1]) + 0.02 * H_spread, x1 = (svnm[x][2]) - 0.02 * H_spread, y0 = 0.7, length = 0.1, code = 3)
  ))
text(x = mean(svnm[c(2, 4)]), y = 0.7, label = expression(italic("H"[spread])), pos = 1)
text(x = mean(svnm[c(4, 7)]), y = 0.7, label = expression(italic("1.5 * H"[spread])), pos = 1)
text(x = mean(svnm[c(2, 6)]), y = 0.7, label = expression(italic("1.5 * H"[spread])), pos = 1)
text(x = mean(svnm[c(1, 6)]), y = 0.7, label = "Outliers", pos = 1)	
}

#fat <- PASWR::Bodyfat[["fat"]]
#showboxplot(fat)