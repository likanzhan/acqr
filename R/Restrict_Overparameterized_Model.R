#' Restrict Over-parameterized model
#' Restrict the overparameterized model into appropriate one
#' @export

Restrict_Overparameterized_Model <-
  function(
             RowLevel = 2,
             ColLevel = 3,
             Dummy = TRUE,
             RowNumber = 1,
             ColNumber = 1) {
    Baseline <- ifelse(Dummy, 0, -1)
    RowData <- gl(RowLevel, RowNumber)
    ColData <- gl(ColLevel, ColNumber)
    RowLen <- nlevels(RowData)
    ColLen <- nlevels(ColData)
    RowCol <- expand.grid(C = ColData, R = RowData)
    RowCol <- RowCol[, c(2, 1)]
    rownames(RowCol) <- paste("mu", RowCol[, "R"], RowCol[, "C"], sep = "")

    ## Overparameterized model matrix

    RowColOver <- RowCol
    if (RowLen >= 2) {
      for (i in 1:(RowLen)) {
        RowColOver[, paste("R", i, sep = "")] <-
          ifelse(RowColOver[, "R"] == i, 1, 0)
      }
    }
    if (ColLen >= 2) {
      for (j in 1:(ColLen)) {
        RowColOver[, paste("C", j, sep = "")] <-
          ifelse(RowColOver[, "C"] == j, 1, 0)
      }
    }
    if (RowLen >= 2 & ColLen >= 2) {
      for (i in 1:(RowLen)) for (j in 1:(ColLen)) {
          RowColOver[, paste("R", i, "C", j, sep = "")] <-
            RowColOver[, paste("R", i, sep = "")] *
              RowColOver[, paste("C", j, sep = "")]
        }
    }
    res <- list("Over_Parameterized_Coding" = RowColOver)

    # Dummy or Deviation coded Mode matrix

    if (RowLen >= 2) {
      for (i in 1:(RowLen - 1)) {
        RowCol[, paste("R", i, sep = "")] <-
          ifelse(RowCol[, "R"] == RowLen, Baseline,
            ifelse(RowCol[, "R"] == i, 1, 0)
          )
      }
    }
    if (ColLen >= 2) {
      for (j in 1:(ColLen - 1)) {
        RowCol[, paste("C", j, sep = "")] <-
          ifelse(RowCol[, "C"] == ColLen, Baseline,
            ifelse(RowCol[, "C"] == j, 1, 0)
          )
      }
    }
    if (RowLen >= 2 & ColLen >= 2) {
      for (i in 1:(RowLen - 1)) for (j in 1:(ColLen - 1)) {
          RowCol[, paste("R", i, "C", j, sep = "")] <-
            RowCol[, paste("R", i, sep = "")] *
              RowCol[, paste("C", j, sep = "")]
        }
    }

    mm <- RowCol[, -c(1:2)]
    mm <- cbind(mu0 = 1, mm)
    mminv <- solve(mm)

    name <- ifelse(Dummy, "Dummy_Coded", "Deviation_Coded")
    res[[paste(name, "Row_Col", sep = "_")]] <- RowCol
    res[[paste(name, "Model_Matrix", sep = "_")]] <- mm
    res[[paste(name, "Inversed_Model_Matrix", sep = "_")]] <- mminv

    return(res)
  }
