#Kenward-Roger approximation
# functions to obtain Denominator degrees of freedom of Kenward-Roger approximation 
# 
#' @keywords internal
#' 
#' @importFrom stats model.frame sigma update vcov
#' @importFrom nlme getVarCov
#' @importFrom methods as


 
# from lmerTest:::contest1D.lmerModLmerTest
get_KR1D <- function(model, L) {
  # Compute var(contrast) and ddf using KR-method via the pbkrtest package
  if(!getME(model, "is_REML"))
    stop("Kenward-Roger's method is only available for REML model fits",
         call.=FALSE)
  if(!requireNamespace("pbkrtest", quietly = TRUE))
    stop("pbkrtest package required for Kenward-Roger's method",
         call.=FALSE)
  ## Add warning as faulty results have been seen with R version 3.3.2 cf https://github.com/hojsgaard/pbkrtest/issues/1
  ## It may also be related to the Matrix version: an unstated dependency in pbkrtest.
  if(getRversion() < "3.3.2")
    warning("Kenward-Roger may give faulty results with R <= 3.3.2")
  vcov_beta_adj <- try(vcovAdj(model), silent=TRUE) # Adjusted vcov(beta)
  if(inherits(vcov_beta_adj, "try-error")) return(list(error=TRUE))
  var_con_adj <- qform(L, as.matrix(vcov_beta_adj)) 
  ddf <- try(Lb_ddf(L=L, V0=vcov(model),
                              Vadj=vcov_beta_adj), silent=TRUE) # vcov_beta_adj need to be dgeMatrix!
  if(inherits(ddf, "try-error")) return(list(error=TRUE))
  list(var_con=var_con_adj, ddf=ddf, error=FALSE)
}



# next functions from mcdonohue/pbkrtest on gitHub 

#-----------------------------------------------------------
# To have adjusted variance-covariance matrix 

vcovAdj <- function(object, details=0){
  UseMethod("vcovAdj")
}

#@
#' rdname kr-vcov
vcovAdj.gls <-function(object, details=0){
  if (!(getME(object, "is_REML"))) {
    object <- update(object, . ~ ., REML = TRUE)
  }
  Phi      <- vcov(object)
  SigmaG   <- get_SigmaG( object, details ) 
  X_star        <- getME(object, "X_star")  
  vcovAdj16_internal( Phi, SigmaG, X_star, details=details)
}


## This is a copy of the '2015' edition
vcovAdj16_internal <- function(Phi, SigmaG, X, details=0){
  
  details=0
  DB <- details > 0 ## debugging only
  t0 <- proc.time()
  
  if (DB){
    cat("vcovAdj16_internal\n")
    cat(sprintf("dim(X) : %s\n", toString(dim(X))))
    print(class(X))
    cat(sprintf("dim(Sigma) : %s\n", toString(dim(SigmaG$Sigma))))
    print(class(SigmaG$Sigma))
  }
  
  
  ##SigmaInv <- chol2inv( chol( forceSymmetric(SigmaG$Sigma) ) )
  SigmaInv <- chol2inv( chol( Matrix::forceSymmetric(as(SigmaG$Sigma, "matrix"))))
  ##SigmaInv <- as(SigmaInv, "dpoMatrix")
  
  if(DB){
    cat(sprintf("Finding SigmaInv: %10.5f\n", (proc.time()-t0)[1] ));
    t0 <- proc.time()
  }
  
  #mat <<- list(SigmaG=SigmaG, SigmaInv=SigmaInv, X=X)
  
  t0 <- proc.time()
  ## Finding, TT, HH, 00
  n.ggamma <- SigmaG$n.ggamma
  TT       <- SigmaInv %*% X
  HH       <- OO <- vector("list", n.ggamma)
  for (ii in 1:n.ggamma) {
    #.tmp <- SigmaG$G[[ii]] %*% SigmaInv
    #HH[[ ii ]] <- .tmp
    #OO[[ ii ]] <- .tmp %*% X
    HH[[ ii ]] <- SigmaG$G[[ii]] %*% SigmaInv
    OO[[ ii ]] <- HH[[ ii ]] %*% X       
  }
  if(DB){cat(sprintf("Finding TT, HH, OO  %10.5f\n", (proc.time()-t0)[1] )); t0 <- proc.time()}
  
  ## Finding PP, QQ
  PP <- QQ <- NULL
  for (rr in 1:n.ggamma) {
    OrTrans <- t( as.matrix(OO[[ rr ]] ))
    PP <- c(PP, list(Matrix::forceSymmetric( -1 * OrTrans %*%  TT)))
    for (ss in rr:n.ggamma) {
      QQ <- c(QQ, list(OrTrans %*% SigmaInv %*% OO[[ss]] ))
    }}
  if(DB){cat(sprintf("Finding PP,QQ:    %10.5f\n", (proc.time()-t0)[1] )); t0 <- proc.time()}
  
  
  ##stat15 <<- list(HH=HH, OO=OO, PP=PP, Phi=Phi, QQ=QQ)
  
  Ktrace <- matrix( NA, nrow=n.ggamma, ncol=n.ggamma )
  for (rr in 1:n.ggamma) {
    HrTrans <- t( as.matrix(HH[[rr]] ))
    for (ss in rr:n.ggamma){
      Ktrace[rr,ss] <- Ktrace[ss,rr]<- sum( HrTrans * HH[[ss]] )
    }}
  if(DB){cat(sprintf("Finding Ktrace:   %10.5f\n", (proc.time()-t0)[1] )); t0 <- proc.time()}
  
  ## Finding information matrix
  IE2 <- matrix( NA, nrow=n.ggamma, ncol=n.ggamma )
  for (ii in 1:n.ggamma) {
    Phi.P.ii <- Phi %*% PP[[ii]]
    for (jj in c(ii:n.ggamma)) {
      www <- .indexSymmat2vec( ii, jj, n.ggamma )
      IE2[ii,jj]<- IE2[jj,ii] <- Ktrace[ii,jj] -
        2 * sum(Phi * QQ[[ www ]]) + sum( Phi.P.ii * ( PP[[jj]] %*% Phi))
    }}
  if(DB){cat(sprintf("Finding IE2:      %10.5f\n", (proc.time()-t0)[1] )); t0 <- proc.time()}
  
  eigenIE2 <- eigen(IE2, only.values=TRUE)$values
  condi    <- min(abs(eigenIE2))
  
  WW <- if (condi > 1e-10) Matrix::forceSymmetric(2 * solve(IE2)) else Matrix::forceSymmetric(2 * ginv(IE2))
  
  ## print("vcovAdj")
  UU <- matrix(0, nrow=ncol(X), ncol=ncol(X))
  ## print(UU)
  for (ii in 1:n.ggamma) {
    for (jj in 1:n.ggamma) {
      www <- .indexSymmat2vec( ii, jj, n.ggamma )
      UU <- UU + WW[ii,jj] * (QQ[[ www ]] - PP[[ii]] %*% Phi %*% PP[[jj]])
    }}
  ## print(UU)
  
  UU <- UU + t(as.matrix(UU))
  ## UU <<- UU
  for (ii in 1:n.ggamma) {
    www <- .indexSymmat2vec( ii, ii, n.ggamma )
    UU<- UU + WW[ii, ii] * (QQ[[ www ]] - PP[[ii]] %*% Phi %*% PP[[ii]])
  }
  ## print(UU)
  GGAMMA <-  Phi %*% UU %*% Phi
  PhiA   <-  Phi + 2 * GGAMMA
  attr(PhiA, "P")     <-PP
  attr(PhiA, "W")     <-WW
  attr(PhiA, "condi") <- condi
  PhiA
}

#-----------------------------------------------------------
# to have data or transformed data for vcovAdj function

getME <- function(object, name, ...){
  UseMethod("getME")
}

getME.gls <- function(object, name, ...){
  # adapated from gls source code

  groups <- object$groups
  glsSt <- object$modelStruct$corStruct
  # model <- object$modelStruct
  for (i in 1:length(sys.parents())){
    if (exists("myformul", envir = parent.frame(n=i), inherits=FALSE)){
      formul <- get("myformul", envir = parent.frame(n=i),  inherits=FALSE)
      data.obj <- get("transformed_data", envir = parent.frame(n=i),  inherits=FALSE)
      }
  }
  # model <- eval(object$call$model, envir=parent.frame(n=4))
  # data.obj <- eval(object$call$data, envir=parent.frame(n=4))
  # mfArgs <- list(formula = asOneFormula(formul, model, groups),
  # data = data.obj)
  mfArgs <- list(formula = formul, data = data.obj)
  
  mfArgs$drop.unused.levels <- TRUE
  dataMod <- do.call(model.frame, mfArgs)
  origOrder <- row.names(dataMod)	# preserve the original order
  if (!is.null(groups)) {
    grps <- groups
    ## ordering data by groups
    ord <- order(grps)
    grps <- grps[ord]
    dataMod <- dataMod[ord, ,drop = FALSE]
    revOrder <- match(origOrder, row.names(dataMod)) # putting in orig. order
    ugroups <- unique(grps)
  } else {
    grps <- data.obj$stim
    ord <- rownames(data.obj)
    ugroups <- unique(data.obj$stim) 
    }

  X_raw <- model.matrix(formul, data=data.obj)
  
  if(name=='X'){
    return(X_raw)
  }
  # if(name=='X_sorted'){
  #   X_sorted <- X_raw[ord,]
  #   return(X_sorted)
  # }
  if(name=='is_REML'){
    return(object$method=='REML')
  }
  # if(name=='Zt'){
  #   # Zt is (n_reff x n_subjects) rows by N cols, e.g. 1940 x 4790
  #   # Pinheiro & Bates p 202
  #   Zt <- matrix(0, nrow=length(ugroups), ncol=nrow(X_raw))
  #   for(i in 1:length(ugroups)){
  #     Zt[i, groups==ugroups[i]] <- t(as.matrix(rep(1, sum(groups==ugroups[i]))))
  #   }
  #   return(Matrix(Zt, sparse=TRUE))
  # }
  if(name=='X_star'){
    # Pinheiro & Bates p 202
    X_sorted <- X_raw[ord,]
    varCov <- lapply(ugroups, function(i) {
      ind <- grps==i
      vw <- 1/varWeights(object$modelStruct$varStruct)[ind]
      vars  <- (object$sigma * vw)^2
      res <- diag(vars)
      res
    } )
    invsqrtLambda <- lapply(ugroups, function(i) solve(.sqrtMat(varCov[[i]]/(sigma( object )^2))))
    X_star   <- matrix(0, nrow=nrow(X_raw), ncol=ncol(X_raw))
    for(i in 1:length(ugroups)){
      X_star[grps==ugroups[i], ] <- t(invsqrtLambda[[i]]) %*% X_sorted[grps==ugroups[i],]
    }
    return(Matrix::Matrix(X_star, sparse=TRUE))
  }
    # if(name=='Zt_star'){
  #   # Pinheiro & Bates p 202
  #   # Zt is (n_reff x n_subjects) rows by N cols, e.g. 1940 x 4790
  #   invsqrtLambda <- lapply(ugroups, function(i) solve(.sqrtMat(getVarCov(object)/(sigma( object )^2))))
  #   # Pinheiro & Bates p 202
  #   Zt_star <- matrix(0, nrow=length(ugroups), ncol=nrow(X_raw))
  #   for(i in 1:length(ugroups)){
  #     Zt_star[i, groups==ugroups[i]] <- t(as.matrix(rep(1, sum(groups==ugroups[i])))) %*% invsqrtLambda[[i]]
  #   }
  #   return(Matrix(Zt_star, sparse=TRUE))
  # }
}

#-----------------------------------------------------------
# to have sigma and others parameters for vcovAdj function

get_SigmaG <- function(object, details=0) {
  UseMethod("get_SigmaG")
}

get_SigmaG.gls  <- function(object, details=0) {
  DB     <- details > 0 ## For debugging only
  if (!.is.lmm(object))
    stop("'object' is not Gaussian linear mixed model")  
  
  # variance of random effects of b's is 0
  GGamma <- 0
  
  ## Put covariance parameters for the random effects into a vector:
  ## Fixme: It is a bit ugly to throw everything into one long vector here; a list would be more elegant
  ggamma <- NULL
  # for ( ii in 1) {
  #   vc     <- GGamma[[1]]
  #   Lii    <- matrix(as.numeric(vc), nrow=1, ncol=1)
  #   ggamma <- c(ggamma, Lii[ lower.tri( Lii, diag=TRUE ) ] )
  # }
  ggamma   <- c( ggamma, sigma( object )^2 ) ## Extend ggamma by the residuals variance
  n.ggamma <- length(ggamma)
  
  ## Find G_r:
  G  <- NULL
  # groups <- object$groups
  # ugroups <- sort(unique(groups))
  # Zt_star <- getME(object, "Zt_star")
  # for (ss in 1) {
  #   ZZ    <- .shget_Zt_group( ss, Zt_star, c(0,length(ugroups)) )
  #   n.lev <- length(ugroups) ## ; cat(sprintf("n.lev=%i\n", n.lev))
  #   Ig    <- sparseMatrix(1:n.lev, 1:n.lev, x=1)
  #   for (rr in 1) {
  #     ## This is takes care of the case where there is random regression and several matrices have to be constructed.
  #     ## FIXME: I am not sure this is correct if there is a random quadratic term. The '2' below looks suspicious.
  #     ii.jj <- .index2UpperTriEntry( rr, 1 ) ##; cat("ii.jj:"); print(ii.jj)
  #     ii.jj <- unique(ii.jj)
  #     if (length(ii.jj)==1){
  #       EE <- sparseMatrix(ii.jj, ii.jj, x=1, dims=rep(1, 2))
  #     } else {
  #       EE <- sparseMatrix(ii.jj, ii.jj[2:1], dims=rep(1, 2))
  #     }
  #     EE <- Ig %x% EE  ## Kronecker product
  #     G  <- c( G, list( t(ZZ) %*% EE %*% ZZ ) )
  #   }
  # }
  
  ## Extend by the indentity for the residual
  n.obs <- nrow(getME(object,'X'))
  G    <- c( G, list(Matrix::sparseMatrix(1:n.obs, 1:n.obs, x=1 )) )
  
  Sigma <- ggamma[1] * G[[1]]
  # for (ii in 2:n.ggamma) {
  #   Sigma <- Sigma + ggamma[ii] * G[[ii]]
  # }
  
  SigmaG <- list(Sigma=Sigma, G=G, n.ggamma=n.ggamma)
  SigmaG
}

#-----------------------------------------------------------
# to calculate ddf of Kenward-Roger

#from pbkrtest::Lb_ddf

Lb_ddf <- function (L, V0, Vadj) 
{
  if (!is.matrix(L)) 
    L = matrix(L, nrow = 1)
  Theta <- t(L) %*% solve(L %*% V0 %*% t(L), L)
  P <- attr(Vadj, "P")
  W <- attr(Vadj, "W")
  A1 <- A2 <- 0
  ThetaV0 <- Theta %*% V0
  n.ggamma <- length(P)
  for (ii in 1:n.ggamma) {
    for (jj in c(ii:n.ggamma)) {
      e <- ifelse(ii == jj, 1, 2)
      ui <- as.matrix(ThetaV0 %*% P[[ii]] %*% V0)
      uj <- as.matrix(ThetaV0 %*% P[[jj]] %*% V0)
      A1 <- A1 + e * W[ii, jj] * (.spur(ui) * .spur(uj))
      A2 <- A2 + e * W[ii, jj] * sum(ui * t(uj))
    }
  }
  q <- nrow(L)
  B <- (1/(2 * q)) * (A1 + 6 * A2)
  g <- ((q + 1) * A1 - (q + 4) * A2)/((q + 2) * A2)
  c1 <- g/(3 * q + 2 * (1 - g))
  c2 <- (q - g)/(3 * q + 2 * (1 - g))
  c3 <- (q + 2 - g)/(3 * q + 2 * (1 - g))
  EE <- 1 + (A2/q)
  VV <- (2/q) * (1 + B)
  EEstar <- 1/(1 - A2/q)
  VVstar <- (2/q) * ((1 + c1 * B)/((1 - c2 * B)^2 * (1 - c3 * 
                                                       B)))
  V0 <- 1 + c1 * B
  V1 <- 1 - c2 * B
  V2 <- 1 - c3 * B
  V0 <- ifelse(abs(V0) < 1e-10, 0, V0)
  rho <- 1/q * (.divZero(1 - A2/q, V1))^2 * V0/V2
  df2 <- 4 + (q + 2)/(q * rho - 1)
  df2
}


ddf_Lb <- function(VVa, Lcoef, VV0=VVa){
  
  if (!is.matrix(Lcoef))
    Lcoef = matrix(Lcoef, ncol = 1)
  
  vlb = sum(Lcoef * (VV0 %*% Lcoef))
  Theta = Matrix::Matrix(as.numeric(outer(Lcoef, Lcoef) / vlb), nrow=length(Lcoef))
  
  P = attr(VVa, "P")
  W = attr(VVa, "W")
  
  A1 = A2 = 0
  ThetaVV0 = Theta%*%VV0
  n.ggamma = length(P)
  for (ii in 1:n.ggamma) {
    for (jj in c(ii:n.ggamma)) {
      e = ifelse(ii==jj, 1, 2)
      ui = as.matrix(ThetaVV0 %*% P[[ii]] %*% VV0)
      uj = as.matrix(ThetaVV0 %*% P[[jj]] %*% VV0)
      A1 =  A1 +  e* W[ii,jj] * (.spur(ui) * .spur(uj))
      A2 =  A2 +  e* W[ii,jj] *  sum(ui * t(uj))
    }}
  
  ## substituted q = 1 in pbkrtest code and simplified
  B  =  (A1 + 6 * A2) / 2
  g  =  (2 * A1 - 5 * A2)  / (3 * A2)
  c1 =  g/(3 + 2 * (1 - g))
  c2 =  (1 - g) / (3 + 2 * (1 - g))
  c3 =  (3 - g) / (3 + 2 * (1 - g))
  EE =  1 + A2
  VV =  2 * (1 + B)
  EEstar  =  1/(1 - A2)
  VVstar  =  2 * ((1 + c1 * B)/((1 - c2 * B)^2  *  (1 - c3 * B)))
  V0 = 1 + c1 * B
  V1 = 1 - c2 * B
  V2 = 1 - c3 * B
  V0 = ifelse(abs(V0) < 1e-10, 0, V0)
  rho  = (.divZero(1 - A2, V1))^2 * V0/V2
  df2  =  4 + 3 / (rho - 1)
  ## cat(sprintf("Lcoef: %s\n", toString(Lcoef)))
  ## cat(sprintf("df2: %f\n", df2))
  df2
}

#-----------------------------------------------------------
# some additional functions used in vcovAdj function

## Alternative to .get_Zt_group
.shget_Zt_group <- function( ii.group, Zt, Gp, ... ){
  zIndex.sub <-  (Gp[ii.group]+1) : Gp[ii.group+1]
  ZZ <- Zt[ zIndex.sub , ]
  return(ZZ)
}


.indexSymmat2vec <- function(i,j,N) {
  ## S[i,j] symetric N times N matrix
  ## r the vector of upper triangular element  in row major order:
  ## r= c(S[1,1],S[1,2]...,S[1,j], S[1,N], S[2,2],...S[N,N]
  ##Result: k: index of k-th element of r
  k <-if (i<=j) {
    (i-1)*(N-i/2)+j
  } else {
    (j-1)*(N-j/2)+i
  }
}


.indexVec2Symmat<-function(k,N) {
  ## inverse of indexSymmat2vec
  ## result: index pair (i,j) with i>=j
  ## k: element in the vector of upper triangular elements
  ## example: N=3: k=1 -> (1,1), k=2 -> (1,2), k=3 -> (1,3), k=4 -> (2,2)
  aa    <- cumsum(N:1)
  aaLow <- c(0,aa[-length(aa)])
  i     <- which(aaLow < k & k <= aa)
  j     <- k - N * i + N - i * (3 - i) / 2 + i
  return(c(i, j))
}


.index2UpperTriEntry <- .indexVec2Symmat


.sqrtMat <- function (A) {
    e <- eigen(A)
    V <- e$vectors
    if (length(e$values) == 1) {
      S <- sqrt(e$values)
    }
    else {
      S <- diag(sqrt(e$values))
    }
    V %*% S %*% t(V)
}


.divZero<-function(x, y, tol=1e-14){
  ## ratio x/y is set to 1 if both |x| and |y| are below tol
  x.y <- if( abs(x)<tol & abs(y)<tol) {
    1
  }
  else {
    x/y
  }
  x.y
}


.is.lmm <- function(object) {
  if (class(object) %in% c("matrix","Matrix")){
    FALSE
  } else {
    class(object) %in% c('lme', 'lmerMod', 'gls')
  }
}


.spur = function(U){
  sum(diag(U))
}




