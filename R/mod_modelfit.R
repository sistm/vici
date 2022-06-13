# Module UI
  
#' @title   mod_modelfit_ui and mod_modelfit_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param data internal
#' @param parent internal
#' @param origin internal
#'
#' @rdname mod_modelfit
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 


mod_modelfit_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("fit"), label = "Fit model",
                 class = "btn-primary")
  )
}
    
# Module Server
    
#' @rdname mod_modelfit
#' @export
#' @keywords internal
    
mod_modelfit_server <- function(input, output, session, datas,parent,origin){
  ns <- session$ns
  # Run whenever fit button is pressed
  
  #browser()
  observeEvent(input$fit, {#crash here
    #browser()
    origin$output$res_error <- reactive("Please select adequate analysis parameters...")
    responses_res <- list()
    boxplot_print <- list()
    heatmap_data2plot <- list()
    toomuchdata <- FALSE
    #browser()

    for(response in parent$selectResponse){
      if(!is.null(datas$df) & parent$selectSubject %in% colnames(datas$df) &
         parent$selectStim %in% colnames(datas$df) & datas$fact_stim_OK &
         (parent$selectArmInter %in% colnames(datas$df) & datas$fact_arm_OK) |
          (parent$selectTimeIntra %in% colnames(datas$df) & datas$fact_time_OK))
       {
        if(parent$selectModel == 1){
          # data tansformation
          if(parent$selectTimeInter != ''){
            #browser()
            data_df <- datas$df[datas$df[, parent$selectTimeInter] == parent$selectRefTimeInter,
                               c(parent$selectSubject, response, parent$selectStim, parent$selectArmInter)]
          }else{
            data_df <- datas$df[, c(parent$selectSubject, response, parent$selectStim, parent$selectArmInter)]
          }
          colnames(data_df) <- c("Subject", "response", "stim", "arm")
          transformed_data <- data_df
          #decalage déjà présent
          transformed_data$bkg <- 0 # intialize bkg ground
          transformed_data <- transformed_data[order(transformed_data$stim, transformed_data$Subject), ] # align stimulations so that subject order matches in the following loop
          if(!is.factor(transformed_data$stim)){
            transformed_data$stim <- as.factor(transformed_data$stim)
          }
          for(l in levels(transformed_data$stim)){
            #browser()
            if(l != parent$selectRefStim){
              transformed_data[transformed_data$stim == l, "bkg"] <- transformed_data[transformed_data$stim == parent$selectRefStim, "response"]
            }
            
          }
          #browser()
          transformed_data$arm <- stats::relevel(factor(transformed_data$arm), ref=parent$selectRefArmInter)
          transformed_data$stim <- stats::relevel(factor(transformed_data$stim), ref=parent$selectRefStim)
          data_df$stim <- relevel(factor(data_df$stim), ref=parent$selectRefStim)
          if(!is.factor(data_df$arm)){
            data_df$arm <- as.factor(data_df$arm)
          }
          # model fit ----
          #browser()
          fit_res <- interarm_fit(transformed_data, parent, response)
          #browser()
          if(!inherits(fit_res$mgls, "try-error")){
            responses_res[[response]]$res_error <- NULL
            responses_res[[response]]$postprocess_res <- interarm_postprocessres(data_df, fit_res)

            boxplot_print[[response]] <- boxplot_VICI(data_df, responses_res[[response]]$postprocess_res$pval_2plot,
                                                      response_name = response, input = parent)
            heatmap_data2plot[[response]] <- responses_res[[response]]$postprocess_res$res_2plot
            heatmap_data2plot[[response]]$response <- response
            heatmap_data2plot[[response]]$pvalue <- cut(heatmap_data2plot[[response]]$pvalue,
                                                        breaks = c(0, 0.001, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 1),
                                                        right = FALSE)
            responses_res[[response]]$res_tab <- fit_res$res_tab
            
          }

        }else if(parent$selectModel == 2){

          # data tansformation
         # browser()
          if(parent$selectArmIntra != ''){
            data_df <- datas$df[datas$df[, parent$selectArmIntra] == parent$selectRefArmIntra,
                               c(parent$selectSubject, response, parent$selectStim, parent$selectTimeIntra)]
          }else{
            data_df <- datas$df[, c(parent$selectSubject, response, parent$selectStim, parent$selectTimeIntra)]
          }
          colnames(data_df) <- c("Subject", "response", "stim", "time")
          #browser()
          data_df$stim <- stats::relevel(factor(data_df$stim), ref=parent$selectRefStim)
          transformed_data <- data_df

          transformed_data$time <- stats::relevel(transformed_data$time, ref=parent$selectRefTimeIntra)
          transformed_data <- try(tidyr::spread(transformed_data, key = "time", value = "response"),
                                  silent = TRUE)

          if(inherits(transformed_data, "try-error")){
            clean_output(output)
            toomuchdata <- TRUE
            origin$output$res_error <- reactive(paste0("Too many observation in time point ", parent$selectRefTimeIntra,
                                                "... Perhaps the Arm to analyzed was not specified"))
          }else{
            for(i in ncol(transformed_data):3){
              transformed_data[, i] <- (transformed_data[, i] - transformed_data[, 3])
            }

            fit_res <- list()
            for(t in 4:ncol(transformed_data)){
              tp <- colnames(transformed_data)[t]
              transformed_data_temp <- transformed_data[, c(1:2, t), drop=FALSE]
              colnames(transformed_data_temp)[3] <- "response"
              transformed_data_temp$bkg <- 0.000 # intialize bkg ground
              #transformed_data_temp$bkg <- as.double(transformed_data_temp$bkg)
              transformed_data_temp <- transformed_data_temp[order(transformed_data_temp$stim,
                                                                   transformed_data_temp$Subject), ] # align stimulations so that subject order matches in the following loop
              for(l in levels(transformed_data_temp$stim)){
                if(l != parent$selectRefStim){
                  transformed_data_temp[transformed_data_temp$stim == l, "bkg"] <- transformed_data_temp[transformed_data_temp$stim ==
                                                                                                           parent$selectRefStim, "response"]
                }
              }
              
              transformed_data_temp$stim <- stats::relevel(factor(transformed_data_temp$stim), ref=parent$selectRefStim)
              # browser()
              # transformed_data$arm <- stats::relevel(factor(transformed_data$arm),ref=parent$selectRefArmInter)# ref=parent$selectRefArmIntra)#
              # transformed_data$stim <- stats::relevel(factor(transformed_data$stim), ref=parent$selectRefStim)
              # data_df$stim <- relevel(factor(data_df$stim), ref=parent$selectRefStim)
              # if(!is.factor(data_df$arm)){
              #   data_df$arm <- as.factor(data_df$arm)
              # }
              # model fit ----
              fit_res[[tp]] <- intraarm_fit(transformed_data = transformed_data_temp,
                                            tested_time = tp, input = parent, resp = response)
            }

            if(!prod(sapply(fit_res, function(x){inherits(x$mgls, "try-error")}))){
              responses_res[[response]]$res_error <- NULL
              responses_res[[response]]$postprocess_res <- intraarm_postprocessres(data_df, fit_res)
              #res_data <<- responses_res[[response]]$postprocess_res
              #responses_res[[response]]$postprocess_res$pval_2plot <- do.call(rbind, responses_res[[response]]$postprocess_res$pval_2plot)
              boxplot_print[[response]] <- boxplot_VICI(data_df, responses_res[[response]]$postprocess_res$pval_2plot,
                                                        response_name = response,
                                                        input = parent,
                                                        inter = FALSE,
                                                        baseline = parent$selectRefTimeIntra)
              responses_res[[response]]$res_tab <- do.call(rbind, lapply(fit_res, "[[", "res_tab"))
              
              heatmap_data2plot[[response]] <- responses_res[[response]]$postprocess_res$res_2plot
              for(l in 1:length(heatmap_data2plot[[response]])){
                heatmap_data2plot[[response]][[l]]$response <- response
                heatmap_data2plot[[response]][[l]]$pvalue <- cut(heatmap_data2plot[[response]][[l]]$pvalue,
                                                                 breaks = c(0, 0.001, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 1),
                                                                 right = FALSE)
              }
              heatmap_data2plot[[response]] <- do.call(rbind.data.frame,
                                                       heatmap_data2plot[[response]])
              #output$res_tab <- renderTable(fit_res$res_tab, rownames = TRUE, digits=5)
            }
          }
        }

      }
      #res_sentence <- renderText("A sentence to be copied and pasted in your analysis report.")
      
    }
    if(!toomuchdata){
      if(length(responses_res)<1){
        clean_output(output)
        origin$output$res_error <- reactive("Please select adequate analysis parameters before trying to fit the model...")
      }else{
        myTabs <- lapply(parent$selectResponse, function(resp) {
          if(is.null(session$userData$res_data)){
            session$userData$res_data<<- responses_res[[resp]]$res_tab
          }else{
            session$userData$res_data<<- rbind(session$userData$res_data,responses_res[[resp]]$res_tab)
          }
          tabPanel(title = resp, value = resp,
                   wellPanel(
                     fluidRow(
                       renderPlot(boxplot_print[[resp]])),
                     h6(""),
                     myDownloadHandlerForPlots(name = "VICIboxplot.png", plot_obj = boxplot_print[[resp]],
                                               outputArgs = list(label = "Download boxplot [PNG]", class = "btn-primary")),
                     h3(""),
                     
                     if(parent$ddf=="Kenward-Roger"){
                       h4(paste("WARNING - Kenward-Roger not implemented, please use SAS to obtain results of this approximation."))
                     } else{
                     h4(paste("Numerical results for", resp))
                       
                       renderTable(
                         # responses_res[[resp]]$res_tab, rownames = TRUE, digits=5)
                         {
                           # browser()
                           responses_res[[resp]]$res_tab[,1] <- formatC(responses_res[[resp]]$res_tab[,1], format="f", digits = 5)
                           responses_res[[resp]]$res_tab[,2] <- formatC(responses_res[[resp]]$res_tab[,2], format="f", digits = 5)
                           if(parent$ddf=="By default"){
                             responses_res[[resp]]$res_tab[,3] <- formatC(as.numeric(substr(responses_res[[resp]]$res_tab[,3], 1,5)), format="f", digits = 0)
                           } else responses_res[[resp]]$res_tab[,3] <- formatC(as.numeric(substr(responses_res[[resp]]$res_tab[,3], 1,4)), format="f", digits = 1)
                           responses_res[[resp]]$res_tab[,4] <- formatC(responses_res[[resp]]$res_tab[,4], format="f", digits = 5)
                           responses_res[[resp]]$res_tab
                         }, rownames=TRUE
                       )
                     }
                   )
          )
        })
        origin$output$boxplotsAndTabs <- renderUI({
          do.call(tabsetPanel, myTabs)
        })

        if(parent$selectModel == 1){
          origin$output$res_nparam <- renderText({paste0("<b>Number of estimated model parameters for each response:</b> ",
                                                  nrow(responses_res[[1]]$res_tab) + ncol(responses_res[[1]]$postprocess_res$vars))})
          heatmap_data2plot_noref <- lapply(heatmap_data2plot, function(x){x[-grep("reference", rownames(x)), ]})
        }else if(parent$selectModel == 2){
          origin$output$res_nparam <- renderText({paste0("<b>Number of estimated model parameters for each response:</b> ",
                                                  nrow(responses_res[[1]]$res_tab) + length(responses_res[[1]]$postprocess_res$vars))})
          heatmap_data2plot_noref <- lapply(heatmap_data2plot, function(x){x[-grep("reference", x$Stimulation), ]})
        }

        origin$output$res_error <- reactive(NULL)
        res_lik_all <- lapply(lapply(responses_res, "[[", "postprocess_res"), "[[", "res_lik")
        res_lik_all <- do.call(rbind.data.frame, res_lik_all)
        #colnames(res_lik_all) <- c("AIC", "-2 Res. logLikelihood")
        origin$output$res_lik <- renderTable(res_lik_all,
                                      rownames = TRUE, digits = 4)
        all_vars <- lapply(lapply(responses_res, "[[", "postprocess_res"), "[[", "vars")
        all_vars <- do.call(rbind.data.frame, all_vars)
        #colames(all_vars) <- levels(data_df$stim)
        origin$output$res_var <- renderTable(all_vars, rownames = TRUE, digits=6)
        #boxplot_dnl <- cowplot::plot_grid(plotlist = boxplot_print)
        #output$boxplot <- renderPlot(boxplot_dnl)
        #output$downloadBP <- myDownloadHandlerForPlots(name = "VICIboxplot.png", plot_obj = boxplot_dnl)
    
        hm_data2plot_all <- do.call(rbind.data.frame, heatmap_data2plot_noref)
        hm_data2plot_all$response <- factor(hm_data2plot_all$response, ordered = TRUE,
                                            levels = rev(parent$selectResponse))
        #TODO if model 2: cross response x time points on y-axis
        if(parent$selectModel == 2)
          heatmap_print <- heatmap_vici(hm_data2plot_all, inter = FALSE,
                                        baseline = parent$selectRefTimeIntra)
        else{
          heatmap_print <- heatmap_vici(hm_data2plot_all, inter = TRUE)
        }
        origin$output$heatmap <- renderPlot(heatmap_print)
        origin$output$downloadHM <- myDownloadHandlerForPlots(name = "VICIheatmap.png", plot_obj = heatmap_print)
      }
    }
    
    updateTabsetPanel(origin, "inTabset", selected = "resTab")
  })
  
}
    
## To be copied in the UI
# mod_modelfit_ui("modelfit_ui_1")
    
## To be copied in the server
# callModule(mod_modelfit_server, "modelfit_ui_1")
 
