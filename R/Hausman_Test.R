#' Conduct a Hausman test
#'
#' Conduct a Hausman test to see if a variable is treated as a fixed effect or a random effect
#'
#' @param glmerMod 
#' @param glmMod 
#' @references 
#' \url{https://stackoverflow.com/questions/23630214/hausmans-specification-test-for-glmer-from-lme4}
#' @export

phtest_glmer <- function (glmerMod, glmMod, ...)  {  ## changed function call
    coef.wi <- coef(glmMod)
    coef.re <- fixef(glmerMod)  ## changed coef() to fixef() for glmer
    vcov.wi <- vcov(glmMod)
    vcov.re <- vcov(glmerMod)
    names.wi <- names(coef.wi)
    names.re <- names(coef.re)
    coef.h <- names.re[names.re %in% names.wi]
    dbeta <- coef.wi[coef.h] - coef.re[coef.h]
    df <- length(dbeta)
    dvcov <- vcov.re[coef.h, coef.h] - vcov.wi[coef.h, coef.h]
    stat <- abs(t(dbeta) %*% as.matrix(solve(dvcov)) %*% dbeta)  ## added as.matrix()
    pval <- pchisq(stat, df = df, lower.tail = FALSE)
    names(stat) <- "chisq"
    parameter <- df
    names(parameter) <- "df"
    alternative <- "one model is inconsistent"
    res <- list(statistic = stat, p.value = pval, parameter = parameter, 
        method = "Hausman Test",  alternative = alternative,
                data.name=deparse(getCall(glmerMod)$data))  ## changed
    class(res) <- "htest"
    return(res)
}