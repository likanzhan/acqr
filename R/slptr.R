slptr <- function(
data = ideal1,
title = "(a). Gender and education are independent",
xlab = "Education",
ylab = "Income"
) {
lancet <- ggsci::pal_lancet("lanonc", alpha = 1)(9)
fmt = lm(Income ~ Education, data = data)
fms = nlme::lmList(Income ~ Education | sex, data = data)
fmsc <- coefficients(fms)
slopet <- as.numeric(coefficients(fmt)[[2]])
degree <- 180 * atan(slopet) / pi
plot(Income ~ Education, data = data, xlab = "", ylab = "",
     xlim = c(min(data[["Education"]]), max(data[["Education"]])),
     axes = FALSE, frame.plot = TRUE, cex = 2, asp = 1,
     pch = c(19, 1)[as.integer(data[, "sex"])],
     col = scales::alpha("black", 0.5))
sapply(1:2, function(x) abline(fmsc[x, 1], fmsc[x, 2], lwd = "2", col = lancet[3]))
abline(fmt, col = lancet[2])
abline(v = mean(data[["Education"]]), lty = 2)
title(xlab = xlab, line = 0.5, font.lab = 2)
title(ylab = ylab,    line = 0.5, font.lab = 2)
text(x = mean(data[["Education"]]), y = mean(fitted(fmt)),
     srt = degree, col = lancet[4], adj = c(0.5, -0.5),
     label = "Income = A + B * Education")
mtext(text = title, side = 3, line = 0.1)
legend("bottomright", pch = c(19, 1), legend = c("Men", "Women"))
}


## Examble data

set.seed(1)
x <- seq(1, 100, by = 1)
y <- x + rnorm(100, 0, 10)
Men   <- data.frame(Education = x, Income = y + 100, sex = "men")
Women <- data.frame(Education = x, Income = y - 100, sex = "women")
ideal2 <- ideal1 <- rbind(Men, Women)
ideal2[, "Education"] <- ifelse(
  ideal2[, "sex"] == "women",
  ideal2[, "Education"] + 100, ideal2[, "Education"])
