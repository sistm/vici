#' @import shiny
#' @import ggpubr
#' @importFrom nlme gls varIdent
#' @importFrom utils read.csv
#' @importFrom stats coef relevel
app_server <- function(input, output,session) {

  # initialize everything ----
  output$mod <- reactive(NULL)
  output$mod_display <- reactive(FALSE)
  output$res_sentence <- reactive(NULL)
  output$res_tab <- reactive(NULL)
  output$res_error <- reactive(NULL)
  output$res_lik <- reactive(NULL)
  output$heatmap <- reactive(NULL)
  output$boxplot <- reactive(NULL)
  output$downloadHM <- reactive(NULL)
  output$downloadBP <- reactive(NULL)
  output$res_var <- reactive(NULL)
  output$armisfactor <- reactive(TRUE)
  output$stimisfactor <- reactive(TRUE)
  output$warningarmisfactor <- reactive(NULL)
  output$warningstimisfactor <- reactive(NULL)
  outputOptions(output, "mod_display", suspendWhenHidden = FALSE)
  outputOptions(output, "warningarmisfactor", suspendWhenHidden = FALSE)
  outputOptions(output, "warningstimisfactor", suspendWhenHidden = FALSE)
  outputOptions(output, "armisfactor", suspendWhenHidden = FALSE)
  outputOptions(output, "stimisfactor", suspendWhenHidden = FALSE)
  outputOptions(output, "res_error", suspendWhenHidden = FALSE)
  outputOptions(output, "res_tab", suspendWhenHidden = FALSE)
  outputOptions(output, "res_lik", suspendWhenHidden = FALSE)
  outputOptions(output, "res_var", suspendWhenHidden = FALSE)
  outputOptions(output, "heatmap", suspendWhenHidden = FALSE)
  outputOptions(output, "downloadHM", suspendWhenHidden = FALSE)
  outputOptions(output, "downloadBP", suspendWhenHidden = FALSE)

  data <- reactiveValues()
  data$fact_stim_OK <- TRUE
  data$fact_arm_OK <- TRUE


  # data import ----
  output$table2render <- DT::renderDataTable({
    req(input$datafile)
    data$df
  },
  options = list(pageLength = 10, lengthMenu = list(c(5, 10, -1), c('5', '10', 'All')))
  )

  observeEvent({input$datafile; input$header; input$sep}, {
    req(input$datafile)
    data$df <- {
      # when reading semicolon separated files,
      # having a comma separator causes `read.csv` to error
      tryCatch(
        {
          df <- utils::read.csv(input$datafile$datapath,
                                header = input$header,
                                sep = input$sep)
        },
        error = function(e){ stop(safeError(e)) } # return a safeError if a parsing error occurs
      )
      clean_output(output)
      output$mod <- reactive(NULL)
      output$mod_display <- reactive(FALSE)
      df}
    available_vars_init <- colnames(data$df)
    updateSelectizeInput(session, "selectSubject",
                         selected = '',
                         choices = c('', available_vars_init),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectResponse",
                         selected = '',
                         choices = c('', available_vars_init),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectStim",
                         selected = '',
                         choices = c('', available_vars_init),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectArm",
                         selected = '',
                         choices = c('', available_vars_init),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectRefArm",
                         selected = ''
    )
    updateSelectizeInput(session, "selectRefStim",
                         selected = ''
    )
    updateTabsetPanel(session, "inTabset", selected = "dataTab")
  })


  # update available variables for selection ----
  observeEvent(data$available_vars, {
    updateSelectizeInput(session, "selectSubject",
                         choices = c(input$selectSubject, data$available_vars, intToUtf8(160)),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectResponse",
                         choices = c(input$selectResponse, data$available_vars, intToUtf8(160)),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectStim",
                         choices = c(input$selectStim, data$available_vars, intToUtf8(160)),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectArm",
                         choices = c(input$selectArm, data$available_vars, intToUtf8(160)),
                         options = list(placeholder = 'Please select a variable below')
    )
  }
  )

  observeEvent(input$selectSubject, {
    if (input$selectSubject != ''){
      data$available_vars <-  setdiff(colnames(data$df), union(union(union(input$selectSubject,
                                                                           input$selectStim),
                                                                     input$selectResponse),
                                                               input$selectArm))
      clean_output(output)
    }
  })

  observeEvent(input$selectStim, {
    if (input$selectStim != ''){
      data$available_vars <-  setdiff(colnames(data$df), union(union(union(input$selectSubject,
                                                                           input$selectStim),
                                                                     input$selectResponse),
                                                               input$selectArm))
      if (input$selectStim %in% colnames(data$df)){
        selected_stim_var <- data$df[, input$selectStim]
        if(is.factor(selected_stim_var)){
          output$stimisfactor <- reactive(TRUE)
          possible_stims <- levels(selected_stim_var)
          output$warningstimisfactor <- reactive(NULL)
          data$fact_stim_OK <- TRUE
        }else{
          output$stimisfactor <- reactive(FALSE)
          output$warningstimisfactor <- reactive(paste0("WARNING: '", input$selectStim, "' is not a factor"))
          data$fact_stim_OK <- FALSE
          possible_stims <- paste0("Error: '", input$selectStim, "' is not a factor\nPlease select a different variable")
        }
        updateSelectizeInput(session, "selectRefStim",
                             choices = c(possible_stims[1], possible_stims),
                             selected = ifelse(input$selectRefStim=='', possible_stims[1], input$selectRefStim)
        )
      }else{
        output$stimisfactor <- reactive(FALSE)
        output$warningstimisfactor <- reactive(paste0("WARNING: '", input$selectStim, "' is not a column in the input data"))
        data$fact_stim_OK <- FALSE
      }
      clean_output(output)
    }
  })

  observeEvent(input$selectResponse, {
    if (input$selectResponse != ''){
      data$available_vars <-  setdiff(colnames(data$df), union(union(union(input$selectSubject,
                                                                           input$selectStim),
                                                                     input$selectResponse),
                                                               input$selectArm))
      clean_output(output)
    }
  })

  observeEvent(input$selectArm, {
    if (input$selectArm != ''){
      data$available_vars <-  setdiff(colnames(data$df), union(union(union(input$selectSubject,
                                                                           input$selectStim),
                                                                     input$selectResponse),
                                                               input$selectArm))
      if (input$selectArm %in% colnames(data$df)){
        selected_arm_var <- data$df[, input$selectArm]
        if(is.factor(selected_arm_var)){
          output$armisfactor <- reactive(TRUE)
          possible_arms <- levels(selected_arm_var)
          output$warningarmisfactor <- reactive(NULL)
          data$fact_arm_OK <- TRUE
          # if(length(possible_arms) > 2){
          #   output$armisfactor <- reactive(FALSE)
          #   output$warningarmisfactor <- reactive(paste0("Error: '", input$selectArm, "' has more than 2 levels\n This is not implemented yet"))
          #   data$fact_arm_OK <- FALSE
          # }
        }else{
          output$armisfactor <- reactive(FALSE)
          output$warningarmisfactor <- reactive(paste0("WARNING: '", input$selectArm, "' is not a factor"))
          data$fact_arm_OK <- FALSE
          possible_arms <- paste0("Error: '", input$selectArm, "' is not a factor\nPlease select a different variable")
        }
        updateSelectizeInput(session, "selectRefArm",
                             choices = c(possible_arms[1], possible_arms),
                             selected = ifelse(input$selectRefArm=='', possible_arms[1], input$selectRefArm)
        )
      }else{
        output$armisfactor <- reactive(FALSE)
        output$warningarmisfactor <- reactive(paste0("WARNING: '", input$selectArm, "' is not a column in the input data"))
        data$fact_arm_OK <- FALSE
      }
      clean_output(output)
    }
  })


  observeEvent({input$selectRefArm; input$selectRefStim}, {
    clean_output(output)
  })


  observeEvent(
    # Run whenever reset button is pressed
    input$modelfit, {
      output$res_error <- reactive("Please select adequate analysis parameters...")
      if(!is.null(data$df) & input$selectSubject %in% colnames(data$df) &
         input$selectStim %in% colnames(data$df) &
         input$selectArm %in% colnames(data$df) &
         data$fact_arm_OK & data$fact_stim_OK){
        # data tansformation
        data_df <- data$df[, c(input$selectSubject, input$selectResponse, input$selectStim, input$selectArm)]
        colnames(data_df) <- c("Subject", "response", "stim", "arm")
        transformed_data <- data_df
        transformed_data$bkg <- 0 # intialize bkg ground
        transformed_data <- transformed_data[order(transformed_data$stim, transformed_data$Subject), ] # align stimulations so that subject order matches in the following loop
        for(l in levels(transformed_data$stim)){
          if(l != input$selectRefStim){
            transformed_data[transformed_data$stim == l, "bkg"] <- transformed_data[transformed_data$stim == input$selectRefStim, "response"]
          }
        }
        transformed_data$arm <- stats::relevel(transformed_data$arm, ref=input$selectRefArm)
        transformed_data$stim <- stats::relevel(transformed_data$stim, ref=input$selectRefStim)
        data_df$stim <- relevel(data_df$stim, ref=input$selectRefStim)


        # model fit ----
        if(input$selectModel == 1){
          mgls <- try(nlme::gls(response ~ -1 + stim*arm + bkg,
                                data = transformed_data,
                                #correlation =  nlme::corCompSymm(form= ~ 1 | signal),
                                weights = nlme::varIdent(value = c("1" = 1), form = ~ 1 | stim),
                                method="REML"
          ), silent = TRUE)
          # nlme::lme(fixed = response ~ -1 + signal + nosignal + signal:arm + nosignal:arm + bkg,
          #           data = transformed_data,
          #           random = list(Subject = nlme::pdDiag(form = ~ -1 + signal)),
          #           #correlation =  nlme::corCompSymm(form= ~ -1 + signal | Subject),
          #           weights = nlme::varIdent(form = ~ 1 | signal),
          #           method="REML"
          # )
        }else{
          mgls <- "Not implemented yet, please select another modeling\n"
          class(mgls) <- "try-error"
        }
        if(!inherits(mgls, "try-error")){
          output$res_error <- reactive(NULL)
          m2resloglik <- -2*mgls$logLik
          s_mgls <- summary(mgls)
          aic <- s_mgls$AIC
          var_res <- mgls$sigma^2
          sigmas <- stats::coef(mgls$modelStruct$varStruct, uncons = FALSE, allCoef = TRUE) * mgls$sigma
          vars <- t(cbind(sigmas^2))
          colnames(vars) <- levels(data_df$stim)
          rownames(vars) <- c("Variance")

          # getting coef
          res_tab <- s_mgls$tTable[, c(1,2,4)]
          colnames(res_tab) <- c("Estimate", "Standard error", "p-value")
          output$res_nparam <- renderText({paste0("<b>Number of estimated model parameters:</b> ", nrow(res_tab) + 2)})

          # pretty coef names

          rownames(res_tab)[1] <- paste0("Average response in reference stimulation ", input$selectRefStim,
                                         " in reference arm ", input$selectRefArm)
          nstim <- nlevels(transformed_data$stim)
          for(i in 1:(nstim-1)){
            rownames(res_tab)[1 + i] <- paste0("Average response in stimulation ", levels(transformed_data$stim)[1 + i],
                                               " in reference arm ", input$selectRefArm)
          }
          narm <- nlevels(transformed_data$arm)
          for(i in 1:(narm-1)){
            rownames(res_tab)[nstim + i] <- paste0("Effect of arm ", levels(transformed_data$arm)[1 + i],
                                                   " on response in reference stimulation ", input$selectRefStim)
          }
          rownames(res_tab)[nstim + narm] <- paste0("Effect of reference stimulation ", input$selectRefStim, " on response")
          for(i in 1:(nstim-1)){
            for(j in 1:(narm-1))
              rownames(res_tab)[narm + nstim + (i-1)*(narm-1) + j] <- paste0("Effect of arm ", levels(transformed_data$arm)[1 + j],
                                                                             " on response in stimulation ", levels(transformed_data$stim)[1 + i])
          }

          # model output ----
          output$res_tab <- renderTable(res_tab, rownames = TRUE, digits=5)
          output$res_lik <- renderTable(t(c("AIC" = aic, "-2 Res. loglikelihood" = m2resloglik)), digits = 4)

          res_2plot <- res_tab[grep("Effect of arm", rownames(res_tab)), ]
          metainfo_2plot <- do.call(rbind, strsplit(gsub(" reference", "", gsub(" stimulation", "", sapply(strsplit(rownames(res_2plot), "Effect of arm "), "[", 2))),
                                                    " on response in "))
          res_2plot <- cbind.data.frame("Arm" = metainfo_2plot[, 1],
                                        "Stimulation" = metainfo_2plot[, 2],
                                        "pvalue" = res_2plot[, 3])


          pval_2plot <- make_nice_pvals(res_2plot, data_df)

          boxplot_print <- boxplot_VICI(data_df, pval_2plot)
          output$boxplot <- renderPlot(boxplot_print)
          output$downloadBP <- myDownloadHandlerForPlots(name = "VICIboxplot.png", plot_obj = boxplot_print)

          res_2plot$pvalue <- cut(res_2plot$pvalue, breaks = c(0, 0.001, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 1), right = FALSE)
          heatmap_print <- heatmap_vici(res_2plot)
          output$heatmap <- renderPlot(heatmap_print)
          output$downloadHM <- myDownloadHandlerForPlots(name = "VICIheatmap.png", plot_obj = heatmap_print)

          output$res_var <- renderTable(vars, rownames = TRUE, digits=6)#t(vars), rownames = TRUE, digits=6)
          output$res_sentence <- renderText("A sentence to be copied and pasted in your analysis report.")
        }else{
          output$res_error <- reactive(paste0("Model was not able to run with the following error message:\n\n", mgls[1],
                                              "\nMake sure analysis parameters are correct"))
          clean_output(output)
        }
      }else{
        output$res_error <- reactive("Please select adequate analysis parameters before trying to fit the model...")
      }
      updateTabsetPanel(session, "inTabset", selected = "resTab")
    })

  observeEvent({input$selectModel; input$selectStim; input$selectRefStim; input$selectArm; input$selectRefArm}, {

    # write LaTeX model ----

    if(input$selectModel == 1 & input$selectRefStim != '' & input$selectRefArm != '' & input$selectStim !='' &
       input$selectArm %in% colnames(data$df) & input$selectStim %in% colnames(data$df)){
      output$mod_display <- reactive(TRUE)
      arm_coefs <- NULL
      for(a in levels(data$df[, input$selectArm])){
        if(a != input$selectRefArm){
          arm_coefs <- paste0(arm_coefs, '+ \\beta_{', a,'}^{', input$selectRefStim, '}', a,
                              '_i')
        }
      }
      statmodel <- paste0('$$y_i^{', input$selectRefStim, '} = \\beta_0^{', input$selectRefStim,
                          '}', arm_coefs, '+ \\varepsilon_i^{', input$selectRefStim, '}$$')
      for(s in levels(data$df[, input$selectStim])){

        if(s != input$selectRefStim){
          for(a in levels(data$df[, input$selectArm])){
            arm_coefs <- NULL
            if(a != input$selectRefArm){
              arm_coefs <- paste0(arm_coefs, '+ \\beta_{', a,'}^{', s, '}', a,
                                  '_i')
            }
          }
          statmodel <- paste0(statmodel, '$$y_i^{', s, '} = \\beta_0^{', s, '} ',
                              arm_coefs, '+ \\beta_{', input$selectRefStim, '}^{', s, '} y^{',
                              input$selectRefStim, '}_i + \\varepsilon_i^{', s, '}$$'
          )
        }
      }
      output$mod <- renderUI({
        withMathJax(statmodel)
      })
    }else if (input$selectModel == 2) {
      output$mod_display <- reactive(TRUE)
      output$mod <- renderUI({
        withMathJax(
          helpText('not implemented yet')
        )
      })
    }else{
      output$mod <- reactive(NULL)
      output$mod_display <- reactive(FALSE)
    }

    clean_output(output)
  }
  )
}
