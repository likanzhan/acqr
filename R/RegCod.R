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
RowCol <- expand.grid(C = ColData, R = RowData)
RowCol <- RowCol[, c(2, 1)]
rownames(RowCol) <- paste("mu", RowCol[, "R"], RowCol[, "C"],  sep = "")
if (RowLen >= 2){
for (i in 1:(RowLen - 1)){
RowCol[, paste("R", i, sep = "")] <-
ifelse(RowCol[, "R"] == RowLen, Baseline,
ifelse(RowCol[, "R"] == i, 1, 0))
}	
}
if (ColLen >= 2){
for (j in 1:(ColLen - 1)){
RowCol[, paste("C", j,  sep = "")] <-
ifelse(RowCol[, "C"] == ColLen, Baseline,
ifelse(RowCol[, "C"] == j, 1, 0))
}	
}
if (RowLen >=2 & ColLen >= 2){
for (i in 1:(RowLen - 1)) for (j in 1:(ColLen - 1)){
RowCol[, paste("R", i, "C", j, sep = "")] <-
RowCol[, paste("R", i, sep = "")] * 
RowCol[, paste("C", j, sep = "")]
}	
}

mm <- RowCol[, -c(1:2)]
mm <- cbind(mu0 = 1, mm)
mminv <- solve(mm)

res <- list(RowCol = RowCol, mm = mm, mminv = mminv)
return(res)
}
