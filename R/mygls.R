mygls <- function (model, data = sys.frame(sys.parent()), correlation = NULL, 
          weights = NULL, subset, method = c("REML", "ML"), na.action = na.fail, 
          control = list(), verbose = FALSE) 
{
  library(nlme)
  Call <- match.call()
  controlvals <- glsControl()
  if (!missing(control)) 
    controlvals[names(control)] <- control
  if (!inherits(model, "formula") || length(model) != 3L) {
    stop("\nmodel must be a formula of the form \"resp ~ pred\"")
  }
  method <- match.arg(method)
  REML <- method == "REML"
  groups <- if (!is.null(correlation)) 
    getGroupsFormula(correlation)
  glsSt <- glsStruct(corStruct = correlation, varStruct = varFunc(weights))
  model <- terms(model, data = data)
  mfArgs <- list(formula = asOneFormula(formula(glsSt), model, 
                                        groups), data = data, na.action = na.action)
  if (!missing(subset)) {
    mfArgs[["subset"]] <- asOneSidedFormula(Call[["subset"]])[[2L]]
  }
  mfArgs$drop.unused.levels <- TRUE
  dataMod <- do.call(model.frame, mfArgs)
  origOrder <- row.names(dataMod)
  if (!is.null(groups)) {
    groups <- eval(substitute(~1 | GR, list(GR = groups[[2L]])))
    grps <- getGroups(dataMod, groups, level = length(getGroupsFormula(groups, 
                                                                       asList = TRUE)))
    ord <- order(grps)
    grps <- grps[ord]
    dataMod <- dataMod[ord, , drop = FALSE]
    revOrder <- match(origOrder, row.names(dataMod))
  }
  else grps <- NULL
  X <- model.frame(model, dataMod)
  contr <- lapply(X, function(el) if (inherits(el, "factor")) 
    contrasts(el))
  contr <- contr[!unlist(lapply(contr, is.null))]
  X <- model.matrix(model, X)
  if (ncol(X) == 0L) 
    stop("no coefficients to fit")
  y <- eval(model[[2L]], dataMod)
  N <- nrow(X)
  p <- ncol(X)
  parAssign <- attr(X, "assign")
  fTerms <- terms(as.formula(model), data = data)
  namTerms <- attr(fTerms, "term.labels")
  if (attr(fTerms, "intercept") > 0) {
    namTerms <- c("(Intercept)", namTerms)
  }
  namTerms <- factor(parAssign, labels = namTerms)
  parAssign <- split(order(parAssign), namTerms)
  fixedSigma <- (controlvals$sigma > 0)
  attr(glsSt, "conLin") <- list(Xy = array(c(X, y), c(N, ncol(X) + 
                                                        1L), list(row.names(dataMod), c(colnames(X), deparse(model[[2]])))), 
                                dims = list(N = N, p = p, REML = as.integer(REML)), logLik = 0, 
                                sigma = controlvals$sigma, fixedSigma = fixedSigma)
  glsEstControl <- controlvals["singular.ok"]
  glsSt <- Initialize(glsSt, dataMod, glsEstControl)
  parMap <- attr(glsSt, "pmap")
  numIter <- numIter0 <- 0L
  repeat {
    oldPars <- c(attr(glsSt, "glsFit")[["beta"]], coef(glsSt))
    if (length(coef(glsSt))) {
      optRes <- if (controlvals$opt == "nlminb") {
        nlminb(c(coef(glsSt)), function(glsPars) -logLik(glsSt, 
                                                         glsPars), control = list(trace = controlvals$msVerbose, 
                                                                                  iter.max = controlvals$msMaxIter))
      }
      else {
        optim(c(coef(glsSt)), function(glsPars) -logLik(glsSt, 
                                                        glsPars), method = controlvals$optimMethod, 
              control = list(trace = controlvals$msVerbose, 
                             maxit = controlvals$msMaxIter, reltol = if (numIter == 
                                                                         0L) controlvals$msTol else 100 * .Machine$double.eps))
      }
      coef(glsSt) <- optRes$par
    }
    else {
      optRes <- list(convergence = 0)
    }
    attr(glsSt, "glsFit") <- glsEstimate(glsSt, control = glsEstControl)
    if (!needUpdate(glsSt)) {
      if (optRes$convergence) 
        stop(optRes$message)
      break
    }
    numIter <- numIter + 1L
    glsSt <- update(glsSt, dataMod)
    aConv <- c(attr(glsSt, "glsFit")[["beta"]], coef(glsSt))
    conv <- abs((oldPars - aConv)/ifelse(aConv == 0, 1, aConv))
    aConv <- c(beta = max(conv[1:p]))
    conv <- conv[-(1:p)]
    for (i in names(glsSt)) {
      if (any(parMap[, i])) {
        aConv <- c(aConv, max(conv[parMap[, i]]))
        names(aConv)[length(aConv)] <- i
      }
    }
    if (verbose) {
      cat("\nIteration:", numIter)
      cat("\nObjective:", format(optRes$value), "\n")
      print(glsSt)
      cat("\nConvergence:\n")
      print(aConv)
    }
    if (max(aConv) <= controlvals$tolerance) {
      break
    }
    if (numIter > controlvals$maxIter) {
      stop("maximum number of iterations reached without convergence")
    }
  }
  glsFit <- attr(glsSt, "glsFit")
  namBeta <- names(glsFit$beta)
  attr(glsSt, "fixedSigma") <- fixedSigma
  attr(parAssign, "varBetaFact") <- varBeta <- glsFit$sigma * 
    glsFit$varBeta * sqrt((N - REML * p)/(N - p))
  varBeta <- crossprod(varBeta)
  dimnames(varBeta) <- list(namBeta, namBeta)
  Fitted <- fitted(glsSt)
  if (!is.null(grps)) {
    grps <- grps[revOrder]
    Fitted <- Fitted[revOrder]
    Resid <- y[revOrder] - Fitted
    attr(Resid, "std") <- glsFit$sigma/varWeights(glsSt)[revOrder]
  }
  else {
    Resid <- y - Fitted
    attr(Resid, "std") <- glsFit$sigma/varWeights(glsSt)
  }
  names(Resid) <- names(Fitted) <- origOrder
  apVar <- if (controlvals$apVar) 
    glsApVar(glsSt, glsFit$sigma, .relStep = controlvals[[".relStep"]], 
             minAbsPar = controlvals[["minAbsParApVar"]], natural = controlvals[["natural"]])
  else "Approximate variance-covariance matrix not available"
  dims <- attr(glsSt, "conLin")[["dims"]]
  dims[["p"]] <- p
  #attr(glsSt, "conLin") <- NULL Let's keep this as we need it !!!
  attr(glsSt, "glsFit") <- NULL
  attr(glsSt, "fixedSigma") <- fixedSigma
  grpDta <- inherits(data, "groupedData")
  structure(class = "gls", list(modelStruct = glsSt, dims = dims, 
                                contrasts = contr, coefficients = glsFit[["beta"]], varBeta = varBeta, 
                                sigma = if (fixedSigma) controlvals$sigma else glsFit$sigma, 
                                apVar = apVar, logLik = glsFit$logLik, numIter = if (needUpdate(glsSt)) numIter else numIter0, 
                                groups = grps, call = Call, method = method, fitted = Fitted, 
                                residuals = Resid, parAssign = parAssign, na.action = attr(dataMod, 
                                                                                           "na.action")), namBetaFull = colnames(X), units = if (grpDta) 
                                                                                             attr(data, "units"), labels = if (grpDta) 
                                                                                               attr(data, "labels"))
}
