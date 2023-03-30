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

#from lmerTest:::contest1D.lmerModLmerTest
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
  
  
  if(method == "Between-Within"){
    
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

qform <- function (L, V){
  sum(L * (V %*% L))
}


# use glsEstimate and to compute the FULL deviance
# adapted from pbkrtest:::devfun_vp
devfun_gls <- function(varpar, gls_obj){
  nvarpar <- length(varpar)
  coef(gls_obj$modelStruct) <- varpar[-nvarpar]
  attr(gls_obj$modelStruct, "conLin")$sigma <- varpar[nvarpar]
  contr <- gls_obj$call$control
  if(is.null(contr)){
    contr <- list(singular.ok = FALSE)
  }
#   
  est <- glsEstimate(object = gls_obj$modelStruct, control = contr)
  return(as.numeric(-2*est$logLik))
}

# mix above with pbkrtest:::get_covbeta
varBetafun_gls <- function(varpar, gls_obj){
  REML <-  gls_obj$dims$REML
  nvarpar <- length(varpar)
  coef(gls_obj$modelStruct) <- varpar[-nvarpar]
  N <- gls_obj$dims$N
  p <- gls_obj$dims$p
  attr(gls_obj$modelStruct, "conLin")$sigma <- varpar[nvarpar]
  contr <- gls_obj$call$control
  if(is.null(contr)){
    contr <- list(singular.ok = FALSE)
  }
  est <- glsEstimate(object = gls_obj$modelStruct, control = contr)
  varBeta <- crossprod(est$sigma * est$varBeta * sqrt((N - REML * p)/(N - p)))
  return(varBeta)
}

#from pbkrtest:::compute_auxillary
compute_jaclist <- function (object, tol = 1e-06){
  if (!inherits(object, "gls")) 
    stop("'model' not an 'gls'")
  
  out <- list(sigma = NULL, vcov_beta = NULL, vcov_varpar = NULL, 
              jacobian_list = NULL)
  out$sigma <- sigma(object)
  out$vcov_beta <- as.matrix(vcov(object))
  
  varpar_opt <- c(coef(object$modelStruct), "sigma" = sigma(object))
  h <- hessian(func = devfun_gls, x = varpar_opt,
                         gls_obj = object)
  
  eig_h <- eigen(h, symmetric = TRUE)
  evals <- eig_h$values
  neg <- evals < -tol
  pos <- evals > tol
  zero <- evals > -tol & evals < tol
  if (sum(neg) > 0) {
    evals_num <- paste(sprintf("%1.1e", evals[neg]), collapse = " ")
    warning(sprintf("Model failed to converge with %d negative eigenvalue(s): %s", 
                    sum(neg), evals_num), call. = FALSE)
  }
  if (sum(zero) > 0) {
    evals_num <- paste(sprintf("%1.1e", evals[zero]), collapse = " ")
    warning(sprintf("Model may not have converged with %d eigenvalue(s) close to zero: %s", 
                    sum(zero), evals_num))
  }
  pos <- eig_h$values > tol
  q <- sum(pos)
  h_inv <- with(eig_h, {
    vectors[, pos, drop = FALSE] %*% diag(1/values[pos], 
                                          nrow = q) %*% t(vectors[, pos, drop = FALSE])
  })
  out$vcov_varpar <- 2 * h_inv
  
  jac <- jacobian(func = varBetafun_gls, x = varpar_opt,
                            gls_obj = object)
  
  out$jacobian_list <- lapply(1:ncol(jac), function(i){array(jac[, i], dim = rep(length(coef(object)), 2))})
  return(out)
}
