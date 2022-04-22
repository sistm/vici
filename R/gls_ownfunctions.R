#' Functions to obtain coefficient, degree of freedom, p-value
#' 
#' This function allows to calculate the different approximations of degrees of 
#' freedom and returns the table of results in the app.
#' 
#' @param model a \code{gls} model.
#' @param ddf degrees of freedom approximation.
#' 
#' @return a matrix containing coefficient, degrees of freedom and p-value 
#' @keywords internal
#' 
#' @importFrom stats vcov sigma pt 
#' @importFrom nlme glsEstimate coef<-
#' @importFrom numDeriv hessian jacobian

get_coefmat_gls <- function (model, ddf = c("Satterthwaite", "Kenward-Roger", "Between-Within")) {
  ddf <- match.arg(ddf)
  p <- length(model$coefficients)
  if (p < 1){
    tab <- as.matrix(contest1D(model, L = numeric(0L), ddf = ddf))
  }else{
    Lmat <- diag(p)
    tab <- rbindall(lapply(1:p, function(i) contest1D(model, L = Lmat[i, ], ddf = ddf)))
    rownames(tab) <- names(model$coefficients)
    as.matrix(tab)
  }
  return(tab)
}

#adapted from lmerTest:::contest1D.lmerModLmerTest
contest1D <- function (model, L, rhs = 0, ddf = c("Satterthwaite", "Kenward-Roger",  "Between-Within"), 
                       confint = FALSE, level = 0.95, ...){
  mk_ttable <- function(estimate, se, ddf) {
    tstat <- (estimate - rhs)/se
    pvalue <- 2 * pt(abs(tstat), df = ddf, lower.tail = FALSE)
    if (confint) {
      ci <- waldCI(estimate, se, ddf, level = level)
      data.frame(Estimate = estimate, `Std. Error` = se, 
                 df = ddf, `t value` = tstat, lower = unname(ci[, 
                                                                "lower"]), upper = unname(ci[, "upper"]), `Pr(>|t|)` = pvalue, 
                 check.names = FALSE)
    }
    else data.frame(Estimate = estimate, `Std. Error` = se, 
                    df = ddf, `t value` = tstat, `Pr(>|t|)` = pvalue, 
                    check.names = FALSE)
  }
  method <- match.arg(ddf)
  if (is.matrix(L)) 
    L <- drop(L)
  stopifnot(is.numeric(L), length(L) == length(model$coefficients), 
            is.numeric(rhs), length(rhs) == 1L)
  if (length(L) == 0L) {
    o <- numeric(0L)
    return(mk_ttable(o, o, o))
  }
  if (any(is.na(L))) 
    return(mk_ttable(NA_real_, NA_real_, NA_real_))
  
  
  estimate <- sum(L * model$coefficients)
  var_con <- sum(L * (model$varBeta %*% L))
  
  
  # if (method == "Kenward-Roger") {
  #   # browser()
  #   ans <- get_KR1D(model, L)
  #   if (!ans$error) {
  #     return(mk_ttable(estimate = estimate, se = sqrt(ans$var_con), 
  #                      ddf = ans$ddf))
  #   }
  #   else {
  #     warning("Unable to compute Kenward-Roger t-test: using Satterthwaite instead", 
  #             call. = FALSE)
  #     if (!inherits(model, "gls")) 
  #       stop("'model' not a 'gls'")
  #   }
  # }  else 
    if(method == "Between-Within"){
    
    # browser()
    return(mk_ttable(estimate = estimate, se = sqrt(var_con), 
                       ddf = ddf_BW(model, L)))
  }
  

  #To have objects of compute_jaclist function
  jaclist <- compute_jaclist(object=model, tol=1e-14)
  grad_var_con <- vapply(jaclist$jacobian_list, function(x) qform(L, x), numeric(1L))
  satt_denom <- qform(grad_var_con, jaclist$vcov_varpar)
  ddf <- drop(2 * var_con^2/satt_denom)
  mk_ttable(estimate = estimate, se = sqrt(var_con), ddf = ddf)
}
