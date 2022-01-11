#' Between-Within functions to obtain Denominator degrees of freedom
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
      # formul <- get("myformul", envir = parent.frame(n=i),  inherits=FALSE)
      data.obj <- get("transformed_data", envir = parent.frame(n=i),  inherits=FALSE)
    }
    if (exists("input", envir = parent.frame(n=i), inherits=FALSE)){
      input.obj <- get("input", envir = parent.frame(n=i),  inherits=FALSE)
      
    }
  }
  
  n_indiv <- length(unique(data.obj$Subject))
  
  armRef <- input.obj$selectRefArmInter
  n_obs_armRef <- nrow(data.obj[which(data.obj$arm==armRef),])
  
  n_param_interac <-  length(stringr::str_detect(parameters, ":")[stringr::str_detect(parameters, ":")==TRUE])
  
  n_param_bkg <- length(stringr::str_detect(parameters, "bkg")[stringr::str_detect(parameters, "bkg")==TRUE])
  
  # for inter-arm
  if(input.obj$selectModel == "1"){
    if(stringr::str_detect(est_param, "bkg")){
      #ddf_between
      ddf <-  n_obs - n_indiv - n_param
    }  else if(stringr::str_detect(est_param, ":")){
      #ddf_within for stim
      ddf <-  n_obs - n_obs_armRef - n_param_interac
    } else{
      #ddf_within for stim and arm
      ddf <- n_obs_armRef - n_indiv - (n_param - n_param_interac - n_param_bkg)
    }
  #for intra-arm
  } else if(input.obj$selectModel == "2"){
    ddf <-  n_obs - n_indiv - n_param
  }
  
  ddf
  
}
  