#' Between-Within functions to obtain Denominator degrees of freedom
#' 
#' Internal function to calculate Between-Within degrees of freedom
#' 
#' @param object a \code{gls} model object.
#' @param L a numeric vector of the same length as the number of fixed effects from \code{object}.
#' 
#' @return a \code{number} object
#' 
#' @keywords internal
#' 
#' @importFrom stringr str_detect


ddf_BW <- function(object, L){

  parameters <- names(object$coefficients)
  est_param <- parameters[which(L==1)]
  
  n_obs <- object$dims$N
  n_param <- object$dims$p
  
  for (i in 1:length(sys.parents())){
    if (exists("transformed_data", envir = parent.frame(n=i), inherits=FALSE)){
      data.obj <- get("transformed_data", envir = parent.frame(n=i),  inherits=FALSE)
    }
    if (exists("input", envir = parent.frame(n=i), inherits=FALSE)){
      input.obj <- get("input", envir = parent.frame(n=i),  inherits=FALSE)
      
    }
  }
  
  n_indiv <- length(unique(data.obj$Subject))
  
  armRef <- input.obj$selectRefArmInter
  n_obs_armRef <- nrow(data.obj[which(data.obj$arm==armRef),])
  
  n_param_interac <-  length(str_detect(parameters, ":")[str_detect(parameters, ":")==TRUE])
  
  n_param_bkg <- length(str_detect(parameters, "bkg")[str_detect(parameters, "bkg")==TRUE])
  
  # for inter-arm
  if(input.obj$selectModel == "1"){
    if(str_detect(est_param, "bkg")){
      #ddf_between
      ddf <-  n_obs - n_indiv - n_param
    }else if(str_detect(est_param, ":")){
      #ddf_within for stim
      ddf <-  n_obs - n_obs_armRef - n_param_interac
    }else{
      #ddf_within for stim and arm
      ddf <- n_obs_armRef - n_indiv - (n_param - n_param_interac - n_param_bkg)
    }
  #for intra-arm
  } else if(input.obj$selectModel == "2"){
    ddf <-  n_obs - n_indiv - n_param
  }
  
  return(ddf)
  
}
  