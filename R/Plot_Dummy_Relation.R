#' Plot possible relations between a factor and a quantative variable
#'
#' Illisturate different possible relations between two a factor and a quantative variable
#'
#' @param Adj_X       Difference on the X level
#' @param Adj_Y       Difference on the Y level
#' @param Interaction Wheter the effects of the dummy variable and the quantative variable are interacted
#' @examples
#' Plot_Dummy_Relation()

Plot_Dummy_Relation <- function(
Adj_X = 0,
Adj_Y = -100,
Interaction = 1
) {
  #### 1. Create Idealized Data
    set.seed(1)
    Education_Original <- seq(0, 100, length.out = 100)
    Income_Jitter <- rnorm(length(Education_Original), 0, 10)
    Education_Male   <- Education_Original
    Education_Female <- Education_Original + Adj_X
    Income_Male      <- 1 / Interaction * Education_Original + Income_Jitter
    Income_Female    <- Interaction     * Education_Original + Income_Jitter + Adj_Y 
    Men       <- data.frame(Education = Education_Male,   Income = Income_Male,   Sex = "Men")
    Women     <- data.frame(Education = Education_Female, Income = Income_Female, Sex = "Women")
    Idealized_Data <- rbind(Men, Women)

  #### 2. Find the Fitted lines
   fmt <- lm(Income ~ Education, data = Idealized_Data)
   fms <- nlme::lmList(Income ~ Education | Sex, data = Idealized_Data)
   fmsc <- coefficients(fms)
   slopet <- as.numeric(coefficients(fmt)[[2]])
   degree <- 180 * atan(slopet) / pi
   lancet <- ggsci::pal_lancet("lanonc", alpha = 1)(9)

  #### 3. Do the Plotting
  plot(Income ~ Education,
    data = Idealized_Data, xlab = "", ylab = "",
    xlim = range(Idealized_Data[["Education"]]),
    axes = FALSE, frame.plot = TRUE, cex = 2, 
    asp = 1, # <- Keep slope of the text correct
    pch = c(19, 1)[as.integer(Idealized_Data[, "Sex"])],
    col = scales::alpha("black", 0.7)
  )
  title(xlab = "Education", line = 0.5, font.lab = 2)
  title(ylab = "Income",    line = 0.5, font.lab = 2)
  sapply(1:2, function(x) abline(fmsc[x, 1], fmsc[x, 2], lwd = "2", col = lancet[3]))
  abline(fmt, col = lancet[2])
  abline(v = mean(Idealized_Data[["Education"]]), lty = 2)
  text(
    x = mean(Idealized_Data[["Education"]]), y = mean(fitted(fmt)),
    srt = degree, col = lancet[4], adj = c(0.5, -0.5),
    label = "Income = A + B * Education"
  )
  if (Adj_X == 0) Relation = "NOT related" else Relation = "related"
  mtext(text = bquote("Gender and Education are"~ .(Relation)), side = 3, line = 0.1)
  legend("bottomright", pch = c(19, 1), legend = c("Men", "Women"))
}

