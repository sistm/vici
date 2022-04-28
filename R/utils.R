# Utility Functions for VICI Package
# 
# ----- contained functions : ------
# qform 
# devfun_gls 
# varBetafun_gls 
# compute_jaclist 
# waldCI 
# rbindall

##############################################
# ------------------- qform ------------------
##############################################
#' Compute Quadratic Form 
#' 
#' @param L a numeric vector.
#' @param V a symmetric numeric matrix.
#' 
#' @return a numerical scalar.
#' @keywords internal


qform <- function (L, V){
  sum(L * (V %*% L))
}


##############################################
# ---------------- devfun_gls ----------------
##############################################
#' Compute Full Deviance 
#' 
#' @param varpar variance parameters.
#' @param gls_obj a \code{gls} object.
#' 
#' @return the full deviance, a numerical scalar.
#' @keywords internal
#' 
#' @details This code is adapted from code in \code{devfun_vp} internal function of 
#' \pkg{pbkrtest} package.
#' 

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


##############################################
# -------------- varBetafun_gls --------------
##############################################
#' Compute covariance of Beta for a Generalized Least Squares (\code{GLS}) Model 
#' 
#' @param varpar variance parameters.
#' @param gls_obj a \code{gls} object.
#' 
#' @return covariance of Beta, a numerical scalar.
#' @keywords internal
#' 
#' @details This code is adapted from code in \code{get_covbeta} internal function of 
#'  \pkg{pbkrtest} package.
#' 

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

##############################################
# -------------- compute_jaclist -------------
##############################################
#' Compute_jaclist quantities needed for the Satterthwaite
#' approximation.
#'
#' Computes vcov of variance parameters (theta, sigma), jacobian of
#' each variance parameter etc.
#' 
#' @param object a \code{gls} object.
#' @param tol a tolerance
#' 
#' @return a list.
#' @keywords internal
#' 
#' @details This code is adapted from code in \code{compute_auxillary} internal 
#' function of \pkg{pbkrtest} package.
#' 

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


##############################################
# ------------------ waldCI -----------------
##############################################
#' Compute Wald Confidence Interval
#' 
#' @param estimate an estimated coefficient.
#' @param se standard error of \code{estimate}.
#' @param df degrees of freedom associate to \code{estimate}. \code{df = Inf} is 
#' allowed.
#' @param level level of confidence interval.
#' 
#' @return a matrix of lower and upper confidence interval.
#' @keywords internal
#' @importFrom stats qt
#' 
#' @details This code is greatly inspired by code from the \pkg{lmerTest}
#'     package.
#' 

waldCI <- function(estimate, se, df = Inf, level = 0.95) {
  stopifnot(length(level) == 1,
            is.numeric(level),
            level > 0, level < 1)
  alpha <- (1 - level)/2
  fac <- qt(alpha, df = df, lower.tail = FALSE)
  res <- cbind(lower = estimate - se * fac,
               upper = estimate + se * fac)
  if(!is.null(names(estimate))) rownames(res) <- names(estimate)
  res
}


##############################################
# ----------------- rbindall -----------------
##############################################
#' \code{rbind} Multiple Objects
#'
#' @param ... objects to be \code{rbind}'ed - typically matrices or vectors
#'
#' @keywords internal


rbindall <- function(...) do.call(rbind, ...)
