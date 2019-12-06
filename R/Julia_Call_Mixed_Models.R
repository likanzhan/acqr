#' Fit General Linear and Generalized Linear Mixed models from Julia
#' @import JuliaCall
#' @import lme4

Julia_Mixed_Models <- function(formula, data, fast = FALSE, time = TRUE, family = "Binomial()", model = "glmm") {

  # 1. load packages
  library(JuliaCall)
  library(lme4)

  # 2. set up
  julia_setup()

  # 3. load julia packages
  julia_library("StatsBase")
  julia_library("GLM")
  julia_library("MixedModels")

  # 4. Define the formula
  if (model == "glmm") mixedmodel <- "GeneralizedLinearMixedModel" else mixedmodel <- "LinearMixedModel"
  if (time) time <- "@time" else time <- NULL
  if (fast) fast <- "true" else fast <- "false"

  if (model == "glmm") {
    julia_formula <- paste("fm", "=", time, "fit(", mixedmodel, ",", "formula", ",", "data", ",", family, ",", "wt = length", ",", "fast = ", fast, ")", sep = " ")
  } else if (model == "lmm") {
    julia_formula <- paste("fm", "=", time, "fit(", mixedmodel, ",", "formula", ",", "data", ")", sep = " ")
  } else {
    "Model should either be 'glmm' or 'lmm'"
  }

  # 5. Fitt the model
  julia_assign("formula", formula)
  julia_assign("data", data)
  model <- julia_eval(julia_formula)

  # 6. Model-fit statistics
  loglikelihood <- julia_eval("loglikelihood(fm)")
  aic <- julia_eval("aic(fm)")
  bic <- julia_eval("bic(fm)")
  dof <- julia_eval("dof(fm)")
  nobs <- julia_eval("nobs(fm)")
  deviance <- julia_eval("deviance(fm)")
  # objective <- julia_eval("objective(fm)")

  # 7. Fixed-effects parameter estimates
  coef <- julia_eval("coef(fm)")
  fixef <- julia_eval("fixef(fm)")
  vcov <- julia_eval("vcov(fm)")
  stderror <- julia_eval("stderror(fm)")
  coeftable <- julia_eval("coeftable(fm)")

  # 8. Covariance parameter estimates
  VarCorr <- julia_eval("VarCorr(fm)")
  varest <- julia_eval("varest(fm)")
  sdest <- julia_eval("sdest(fm)")

  # 9. Conditional modes of the random effects
  ranef <- julia_eval("ranef(fm)")
  # condVar <- julia_eval("condVar(fm)")

  # 10. Summary
  table_summary <- cbind(JuliaCall::field(coeftable, "rownms"), as.data.frame(JuliaCall::field(coeftable, "cols")))
  colnames(table_summary) <- c("Fixed Effects", JuliaCall::field(coeftable, "colnms"))

  reslist <- list(
    loglikelihood = loglikelihood, aic = aic, bic = bic, dof = dof, nobs = nobs, deviance = deviance, # objective = objective, # Model_fit_statistics
    coef = coef, fixef = fixef, vcov = vcov, stderror = stderror, coeftable = table_summary, # Fixed_effects_parameter
    VarCorr = VarCorr, varest = varest, sdest = sdest, sdest = sdest, # Covariance_parameter
    ranef = ranef, # Conditional_modes_random_effects
    model = model
  ) # condVar
  class(reslist) <- "Julia_Mixed_Models"
  return(reslist)
}

#' Generalized Linear Mixed Effect Moddels
#' @export
jglmer <- function(formula, data, fast = TRUE, time = FALSE, family = "Binomial()", ...) {
  Julia_Mixed_Models(formula, data, fast = fast, time = time, family = family, model = "glmm")
}

#' Linear Mixed Effect Moddels
#' @export
jlmer <- function(formula, data, time = FALSE, ...) {
  Julia_Mixed_Models(formula, data, time = time, model = "lmm")
}

#' Linear Mixed Effect Moddels
#' @export
print.Julia_Mixed_Models <- function(x) print(x[["coeftable"]])

# attach("~/.julia/packages/MixedModels/vY30Z/test/dat.rda")
