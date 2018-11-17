#' Plot possible relations between two factors
#'
#' Illisturate different possible relations between two factors
#'
#' @param R levels of the row factor
#' @param C levels of the column factor
#' @param vls values to be draw
#' @examples
#' Plot_Factor_Relation()
#'
#' @export

Plot_Factor_Relation <- function(
    R = 2, C = 3,
    vls = 1:(R * C)
    ) {
  RowLevel <- paste("R", 1:R, sep = "")
  ColLevel <- paste("C", 1:C, sep = "")
  data <- expand.grid(
    Row = RowLevel, Col = ColLevel, 
    KEEP.OUT.ATTRS = TRUE, stringsAsFactors = FALSE
  )
  data[, "vls"] <- vls
  pch <- c(1, 19, 21, 0, 15, 22, 5, 18, 23)[1:R]
  with (data, 
  interaction.plot(
      x.factor = Col, 
      trace.factor = Row, 
      response = data$vls, cex = 2, lwd = 2, 
      type = "b", pch = pch, axes = FALSE,
      ylim = c(0, max(data$vls) + 1),      
      yaxt = "n", ylab = "", xlab = "Column", 
      las = 1, xaxs = "i", yaxs = "i"
    )
    )
axis(1, at = c(0, length(unique(data$Col)) + 0.2), 
    labels = c("", ""), lwd.tick = 0)
axis(1, at = 1:length(unique(data$Col)), 
    labels = unique(data$Col), lwd = 0, lwd.ticks = 1)  
axis(2, at = c(0, max(data$vls) + 1), 
    labels = c("", ""), lwd.tick = 0)
mtext(bquote(mu[jk]), line = 0, at = 0.9)
}
#Plot_Factor_Relation()