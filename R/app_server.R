#' @import shiny
#' @import ggpubr
#' @importFrom nlme gls varIdent
#' @importFrom utils read.csv write.table
#' @importFrom stats coef relevel as.formula model.matrix
#' @importFrom tidyr spread
#' @importFrom cowplot plot_grid
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
  output$arm2isfactor <- reactive(TRUE)
  output$timeisfactor <- reactive(TRUE)
  output$time2isfactor <- reactive(TRUE)
  output$stimisfactor <- reactive(TRUE)
  output$warningarmisfactor <- reactive(NULL)
  output$warningarm2isfactor <- reactive(NULL)
  output$warningstimisfactor <- reactive(NULL)
  output$warningtimeisfactor <- reactive(NULL)
  output$warningtime2isfactor <- reactive(NULL)
  outputOptions(output, "mod_display", suspendWhenHidden = FALSE)
  outputOptions(output, "warningarmisfactor", suspendWhenHidden = FALSE)
  outputOptions(output, "warningarm2isfactor", suspendWhenHidden = FALSE)
  outputOptions(output, "warningstimisfactor", suspendWhenHidden = FALSE)
  outputOptions(output, "armisfactor", suspendWhenHidden = FALSE)
  outputOptions(output, "arm2isfactor", suspendWhenHidden = FALSE)
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
  data$fact_time_OK <- TRUE
  data$fact_time2_OK <- TRUE
  
  session$userData$res_data <- NULL



  output$downloadRes <- downloadHandler(
    filename = "ResVICI.txt",
    
    content = function(file){
      utils::write.table(session$userData$res_data,file,row.names = TRUE, sep = "\t", quote = FALSE)
    }
  )
#  library('Rlabkey')
#  observe({
#    query <- parseQueryString(session$clientData$url_search)
#    if (!is.null(query[['key']])) {
#      
#      #updateSliderInput(session, "bins", value = query[['bins']])
#      key <<- query[['key']]
#      subF <<- query[['sub']]
#      assay <<- query[['file']]
#      set <<- paste0("apikey|",key)
#      
#      Rlabkey::labkey.setDefaults(apiKey=set)#"apikey|73ea3ff0973f38d52f5b1bbd8980f62c")
#      Rlabkey::labkey.setDefaults(baseUrl = "https://labk.bph.u-bordeaux.fr/")#(baseUrl="https://labkey.bph.u-bordeaux.fr:8443/")
#      labkey.data <- labkey.selectRows(
#"        baseUrl="https://labk.bph.u-bordeaux.fr", 
#        #folderPath="/EBOVAC/assays/EBL2001/ICS", 
#        folderPath=subF,  #"/VASI/VICI/SISTM",
#        schemaName=paste0("assay.General.",assay),#"assay.General.Vici_Sistm", 
#        queryName="Data", 
#        viewName="", 
#        colSort="", 
#        #colFilter=makeFilter(c("Run/RowId", "EQUAL", "140"),c("Antigen", "NOT_EQUAL_OR_MISSING", "Negative control")), 
#        containerFilter=NULL
#      )
      
      #cat("Result request => ")
      #cat(as.character(labkey.data),"\n")
#      data$df <<- labkey.data
#    }
#  })
  
  #cat("Data: \n")
  #cat(str(data))
  #Module return input so sub module can access it
  inpt <- callModule(module = mod_settings_pan_server, id = "settings_pan_ui_1",data = data,parent = session)

  callModule(module = mod_modelfit_server, id = "modelfit_ui_1",datas = data,parent = inpt,origin = session)
  # cat("before write Latex", "\n")
   observeEvent({
     input$selectModel;
     input$selectStim;
     input$selectRefStim;
     input$selectArmInter; 
     input$selectArmIntra;
     input$selectRefArmInter ;
     input$selectRefArmIntra;
     input$selectTimeInter ;
     input$selectTimeIntra;
     input$selectRefTimeInter ;
     input$selectRefTimeIntra}, {
       #appelé data load

       # write LaTeX model ----
      # cat("can write Latex", "\n")
       
       if(input$selectModel == 1 & input$selectRefStim != '' & input$selectRefArmInter != '' & input$selectStim !='' &
          input$selectArmInter %in% colnames(data$df) & input$selectStim %in% colnames(data$df)){
         # cat("write Latex Inter", "\n")
         output$mod_display <- reactive(TRUE)
         arm_coefs <- NULL
         for(a in levels(data$df[, input$selectArmInter])){
           if(a != input$selectRefArmInter){
             arm_coefs <- paste0(arm_coefs, '+ \\beta_{', a,'}^{', input$selectRefStim, '}', a,
                                 '_i')
           }
         }
         statmodel <- paste0('$$y_i^{', input$selectRefStim, '} = \\beta_0^{', input$selectRefStim,
                             '}', arm_coefs, '+ \\varepsilon_i^{', input$selectRefStim, '}$$')
         for(s in levels(data$df[, input$selectStim])){

           if(s != input$selectRefStim){
             arm_coefs <- NULL
             for(a in levels(data$df[, input$selectArmInter])){
               if(a != input$selectRefArmInter){
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
       }else if(input$selectModel == 2 & input$selectRefStim != '' & input$selectRefTimeIntra != '' & input$selectStim !='' &
              input$selectTimeIntra %in% colnames(data$df) & input$selectStim %in% colnames(data$df)) {
         output$mod_display <- reactive(TRUE)

         statmodel <- NULL
         for(t in levels(data$df[, input$selectTimeIntra])){
           if(t != input$selectRefTimeIntra){
             statmodel <- paste0(statmodel, '$$y_{diff\\,',t ,'\\, _i}^{', input$selectRefStim, '} = \\beta_{0\\,',t ,'}^{', input$selectRefStim,
                                 '} ', '+ \\varepsilon_{',t ,'\\, _i}^{', input$selectRefStim, '}$$')
           }
         }
         for(s in levels(data$df[, input$selectStim])){
           if(s != input$selectRefStim){
             for(t in levels(data$df[, input$selectTimeIntra])){
               if(t != input$selectRefTimeIntra){
                 statmodel <- paste0(statmodel, '$$y_{diff\\,',t ,'\\, _i}^{', s, '} = \\beta_{0\\,',t ,'}^{', s,
                                     '} + \\beta_{', input$selectRefStim, '\\,',t ,'}^{', s, '} \\,y^{',
                                     input$selectRefStim, '}_{diff\\,',t ,'\\, _i} + \\varepsilon_{',t ,'\\, _i}^{', s, '}$$'
                 )
               }
             }
           }
         }
         diffdef <- paste0('where \\(y_{diff\\,\\{\\textsf{t}\\}\\, _i}^{\\{\\textsf{s}\\}} = y_i^{\\{\\textsf{s}\\}}(\\{\\textsf{t}\\}) - y_i^{\\{\\textsf{s}\\}}(',
                           input$selectRefTimeIntra, ')\\)'
         )
         output$mod <- renderUI({
           tagList(
             withMathJax(statmodel),
             div(""),
             div(diffdef)
           )
         })
       }else{
         # cat("no write Latex", "\n")
         output$mod <- reactive(NULL)
         output$mod_display <- reactive(FALSE)
       }
       clean_output(output)
     })
}
