#'Fitting GLS For Intra-Arm Setting
#'
#' @keywords internal
#' @importFrom stats na.omit
#' @importFrom nlme varIdent

intraarm_fit <- function(transformed_data, tested_time, input,resp){
  
  res_tab <- NULL
  res_lik <- NULL
  res_error <- NULL

  bkg_inter_mat <- model.matrix(data = stats::na.omit(transformed_data), ~ -1 + stim:bkg)[, -1, drop=FALSE]
  colnames(bkg_inter_mat) <- gsub(":", "_", colnames(bkg_inter_mat), fixed = TRUE)
  transformed_data <- cbind.data.frame(stats::na.omit(transformed_data), bkg_inter_mat)
  myformul <- as.formula(paste0("response ~ -1 + stim", "+", paste(colnames(bkg_inter_mat), collapse = " + ")))

  mgls <- mygls(myformul,
                  data = transformed_data,
                  # correlation =  nlme::corCompSymm(form= ~ 1 | stim),
                  weights = varIdent(value = c("1" = 1), form = ~ 1 | stim),
                  method="REML", na.action = stats::na.omit
  )

  if(!inherits(mgls, "try-error")){

    # getting coef
    s_mgls <- summary(mgls)
    res_lik <- mgls$logLik
    if(input$ddf == "By default"){
      df_residual <- mgls$dims$N - mgls$dims$p
      res_tab <- data.frame(cbind(s_mgls$tTable[, 1:2], rep(df_residual, nrow(s_mgls$tTable)), s_mgls$tTable[, 4]))
    }else res_tab <- get_coefmat_gls(mgls, ddf=input$ddf)[, c(1,2,3,5)]
    colnames(res_tab) <- c("Estimate", "Standard error", "ddf", "p-value")
    sigmas <- stats::coef(mgls$modelStruct$varStruct, uncons = FALSE, allCoef = TRUE) * mgls$sigma
    res_nparam <- renderText({paste0("<b>Number of estimated model parameters:</b> ", nrow(res_tab) + length(sigmas))})

    # pretty coef names
    rownames(res_tab)[1] <- paste0(as.character(resp), " : Vaccine effect on response in reference stimulation ", input$selectRefStim,
                                   " at ", tested_time, " compared to baseline ", input$selectRefTimeIntra)
    nstim <- nlevels(transformed_data$stim)
    for(i in 1:(nstim-1)){
      rownames(res_tab)[1 + i] <- paste0(as.character(resp), " : Vaccine effect on response in stimulation ", levels(transformed_data$stim)[1 + i],
                                         " at ", tested_time, " compared to baseline ", input$selectRefTimeIntra)
      rownames(res_tab)[nstim + i] <- paste0(as.character(resp), " : Effect of reference stimulation ", input$selectRefStim,
                                             " on response in stimulation ", levels(transformed_data$stim)[1 + i],
                                             "  at ", tested_time, " compared to baseline ", input$selectRefTimeIntra)
    }

  }else{
    res_error <- paste0("Model was not able to run with the following error message:\n\n", mgls[1],
                        "\nMake sure analysis parameters are correct")
  }

  return(list("mgls" = mgls,
              "res_error" = res_error,
              "res_tab" = res_tab,
              "res_lik" = res_lik))
}
