#' @import shiny
#' @import ggpubr
#' @importFrom nlme gls varIdent
#' @importFrom utils read.csv
#' @importFrom stats coef relevel as.formula model.matrix
app_server <- function(input, output, session) {

  # initialize everything ----
  output$mod <- reactive(NULL)
  output$mod_display <- reactive(FALSE)
  output$res_sentence <- reactive(NULL)
  #output$res_tab <- reactive(NULL)
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
  #outputOptions(output, "res_tab", suspendWhenHidden = FALSE)
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
                         selected = input$selectResponse,
                         choices = as.list(c(input$selectResponse, data$available_vars, intToUtf8(160))),
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
    if (length(input$selectResponse) >= 1){
      if (input$selectResponse[1] != ''){
        data$available_vars <-  setdiff(colnames(data$df), union(union(union(input$selectSubject,
                                                                             input$selectStim),
                                                                       input$selectResponse),
                                                                 input$selectArm))
        clean_output(output)
      }
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
      responses_res <- list()
      boxplot_print <- list()
      heatmap_data2plot <- list()
      for(response in input$selectResponse){
        if(!is.null(data$df) & input$selectSubject %in% colnames(data$df) &
           input$selectStim %in% colnames(data$df) &
           input$selectArm %in% colnames(data$df) &
           data$fact_arm_OK & data$fact_stim_OK){
          # data tansformation
          data_df <- data$df[, c(input$selectSubject, response, input$selectStim, input$selectArm)]
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
            fit_res <- interarm_fit(transformed_data, input)
          }else{
            fit_res <- list()
            fit_res$mgls <- "Not implemented yet, please select another modeling\n"
            class(fit_res$mgls) <- "try-error"
          }

          if(!inherits(fit_res$mgls, "try-error")){
            responses_res[[response]]$res_error <- NULL
            responses_res[[response]]$postprocess_res <- interarm_postprocessres(data_df, fit_res)
            boxplot_print[[response]] <- boxplot_VICI(data_df, responses_res[[response]]$postprocess_res$pval_2plot,
                                                      response_name = response)
            heatmap_data2plot[[response]] <- responses_res[[response]]$postprocess_res$res_2plot
            heatmap_data2plot[[response]]$response <- response
            heatmap_data2plot[[response]]$pvalue <- cut(heatmap_data2plot[[response]]$pvalue,
                                                        breaks = c(0, 0.001, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 1),
                                                        right = FALSE)
            responses_res[[response]]$res_tab <- fit_res$res_tab
            #output$res_tab <- renderTable(fit_res$res_tab, rownames = TRUE, digits=5)

          }

        }
        #res_sentence <- renderText("A sentence to be copied and pasted in your analysis report.")
      }

      if(length(responses_res)<1){
        clean_output(output)
        output$res_error <- reactive("Please select adequate analysis parameters before trying to fit the model...")
      }else{
        myTabs <- lapply(input$selectResponse, function(resp) {
          tabPanel(title = resp, value = resp,
                   wellPanel(
                     fluidRow(
                       renderPlot(boxplot_print[[resp]])),
                     h6(""),
                     myDownloadHandlerForPlots(name = "VICIboxplot.png", plot_obj = boxplot_print[[resp]],
                                               outputArgs = list(label = "Download boxplot [PNG]", class = "btn-primary")),
                     h3(""),
                     h4(paste("Numerical results for", resp)),
                     renderTable(responses_res[[resp]]$res_tab, rownames = TRUE, digits=5)
                   )
          )
        })
        output$boxplotsAndTabs <- renderUI({
          do.call(tabsetPanel, myTabs)
        })

        output$res_nparam <- renderText({paste0("<b>Number of estimated model parameters for each response:</b> ",
                                                nrow(responses_res[[1]]$res_tab) + 2)})

        output$res_error <- reactive(NULL)
        res_lik_all <- t(sapply(lapply(responses_res, "[[", "postprocess_res"), "[[", "res_lik"))
        rownames(res_lik_all) <- "AIC"
        output$res_lik <- renderTable(res_lik_all,
                                      rownames = TRUE, digits = 4)
        all_vars <- sapply(lapply(responses_res, "[[", "postprocess_res"), "[[", "vars")
        rownames(all_vars) <- levels(data_df$stim)
        output$res_var <- renderTable(all_vars, rownames = TRUE, digits=6)

        #boxplot_dnl <- cowplot::plot_grid(plotlist = boxplot_print)
        #output$boxplot <- renderPlot(boxplot_dnl)
        #output$downloadBP <- myDownloadHandlerForPlots(name = "VICIboxplot.png", plot_obj = boxplot_dnl)

        heatmap_data2plot_noref <- lapply(heatmap_data2plot, function(x){x[-grep("reference", rownames(x)), ]})
        hm_data2plot_all <- do.call(rbind.data.frame, heatmap_data2plot_noref)
        hm_data2plot_all$response <- factor(hm_data2plot_all$response, ordered = TRUE,
                                            levels = rev(input$selectResponse))
        heatmap_print <- heatmap_vici(hm_data2plot_all)
        output$heatmap <- renderPlot(heatmap_print)
        output$downloadHM <- myDownloadHandlerForPlots(name = "VICIheatmap.png", plot_obj = heatmap_print)
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
