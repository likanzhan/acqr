RegCod <- 
function (
RowLevel = 2, 
ColLevel = 3, 
Dummy = TRUE, 
RowNumber = 1, 
ColNumber = 1
) {
Baseline <- ifelse(Dummy, 0, -1) 
RowData <- gl(RowLevel, RowNumber)
ColData <- gl(ColLevel, ColNumber)
RowLen <- nlevels(RowData)
ColLen <- nlevels(ColData)
RowCol <- expand.grid(C = ColData, R = RowData)[, c(2, 1)]
for (i in 1:(RowLen - 1)){
RowCol[, paste("R", i, sep = "")] <-
ifelse(RowCol[, "R"] == RowLen, Baseline,
ifelse(RowCol[, "R"] == i, 1, 0))
}
for (j in 1:(ColLen - 1)){
RowCol[, paste("C", j,  sep = "")] <-
ifelse(RowCol[, "C"] == ColLen, Baseline,
ifelse(RowCol[, "C"] == j, 1, 0))
}
for (i in 1:(RowLen - 1)) for (j in 1:(ColLen - 1)){
RowCol[, paste("R", i, "C", j, sep = "")] <-
as.numeric(
RowCol[, paste("R", i, sep = "")] * 
RowCol[, paste("C", j, sep = "")] )
}
return(RowCol)
}
