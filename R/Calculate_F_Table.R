#' Calculate_F_Table 
#'
#' @param model
#'
#' @examples
#' Calculate_F_Table()
#'
#' @export

Calculate_F_Table <- function(model) {
# Response_Term <- as.character(formula(model)[2])
# Response <- model[["model"]][[Response_Term]]
Response <- model.response(model.frame(model))
SSR <- sum((predict(model) - mean(Response)) ^ 2)
SSE <- deviance(model)
# SSE <- sum((residuals(model)) ^ 2)
SST <- SSR + SSE
DFR <- model[["rank"]] - 1
DFE <- df.residual(model)
DFT <- DFR + DFE
MSR <- SSR / DFR
MSE <- SSE /DFE
F_value <- MSR / MSE
p_value	<- df(F_value, DFR, DFE)
results <- data.frame(
    Source = c("Regression", "Residuals", "Total"),
    Sum_of_Squares = c(SSR, SSE, SST),
    df = c(DFR, DFE, DFT),
    Mean_Square = c(MSR, MSE, NA),
    F_Value = c(F_value, NA, NA),
    p_Value = c(p_value, NA, NA)
    )
results   
}
