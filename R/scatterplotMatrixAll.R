#' @export

scatterplotMatrixAll <- function(data, ...) {
  car::scatterplotMatrix(
    as.formula(paste("~", paste(colnames(data), collapse = " + "))),
    data = data, ...
  )
}
