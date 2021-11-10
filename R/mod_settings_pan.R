# Module UI
  
#' @title   mod_settings_pan_ui and mod_settings_pan_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param data internal
#' @param parent
#'
#' @rdname mod_settings_pan
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_settings_pan_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarPanel(
      # Input: Select a file ----
      h3("Data input"),
      fileInput(ns("datafile"), label = "Choose a CSV/TXT file to import",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Input: Checkbox if file has header ----
      checkboxInput(ns("header"), "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons(ns("sep"), "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = "\t"),
      
      # Input: Select quotes ----
      # radioButtons("quote", "Quote",
      #              choices = c(None = "",
      #                          "Double Quote" = '"',
      #                          "Single Quote" = "'"),
      #              selected = '"'),
      
      # Horizontal line ----
      # tags$hr(),
      
      h3("Input parameters"),
      selectInput(ns("selectModel"), label = "Model choice",
                  choices = list("inter-arm" = 1, "intra-arm" = 2),
                  selected = 1),
      
      h4("Variable specification"),
      selectizeInput(ns("selectSubject"), label = "Select the column that identifies the subject ID",
                     choices = c(Choose = "", NULL),
                     options = list(placeholder = 'Please select a column name below')
      ),
      
      selectizeInput(ns("selectResponse"), label = "Select the column(s) that identify ICS response",
                     choices = c(Choose = "", NULL),
                     options = list(placeholder = 'Please select a column name below'),
                     multiple = TRUE
      ),
      
      selectizeInput(ns("selectStim"), label = "Select the column that identifies the stimulation",
                     choices = c(Choose = "", NULL),
                     options = list(placeholder = 'Please select a column name below')
      ),
      conditionalPanel(
        condition = sprintf("input['%s']!= '' & !output.stimisfactor",ns("selectStim")),#"input.selectStim != '' & !output.stimisfactor",
        verbatimTextOutput(ns("warningstimisfactor"))
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] != '' & output.stimisfactor",ns("selectStim")),#"input.selectStim != '' & output.stimisfactor",
        selectizeInput(ns("selectRefStim"), label = "Select the value that identifies background samples",
                       choices =c(Choose = "", NULL))
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 1",ns("selectModel")),#"input.selectModel == 1",
        selectizeInput(ns("selectArmInter"), label = "Select the column that identifies the arm",
                       choices = c(Choose = "", NULL),
                       options = list(placeholder = 'Please select a column name below')
        )
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 1 & input['%s'] != '' & !output.armisfactor & output.warningarmisfactor != null",ns("selectModel"),ns("selectArmInter")),#"input.selectModel == 1 & input.selectArmInter != '' & !output.armisfactor & output.warningarmisfactor != null",
        verbatimTextOutput(ns("warningarmisfactor"))
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 1 & input['%s'] != '' & output.armisfactor",ns("selectModel"),ns("selectArmInter")),#"input.selectModel == 1 & input.selectArmInter != '' & output.armisfactor",
        selectizeInput(ns("selectRefArmInter"), label = "Select the value that identifies the reference arm",
                       choices =c(Choose = "", NULL))
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 1",ns("selectModel")),#"input.selectModel == 1",
        selectizeInput(ns("selectTimeInter"), label = "If several time-points (optional), please select the column that identifies the observation's time-point",
                       choices = c(Choose = "", NULL),
                       options = list(placeholder = 'Please select a column name below')
        )
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 1 & input['%s'] != '' ",ns("selectModel"),ns("selectTimeInter")) ,#"input.selectModel == 1 & input.selectTimeInter != '' ",
        selectizeInput(ns("selectRefTimeInter"), label = "Select the time-point to analyze",
                       choices =c(Choose = "", NULL))
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 2",ns("selectModel")),#"input.selectModel == 2",
        selectizeInput(ns("selectTimeIntra"), label = "Select the column that identifies the time-points",
                       choices = c(Choose = "", NULL),
                       options = list(placeholder = 'Please select a column name below')
        )
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 2 & input['%s'] != ''",ns("selectModel"),ns("selectTimeIntra")),#"input.selectModel == 2 & input.selectTimeIntra != ''",
        selectizeInput(ns("selectRefTimeIntra"), label = "Select the value that identifies the reference time-point",
                       choices =c(Choose = "", NULL))
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 2",ns("selectModel")),#"input.selectModel == 2",
        selectizeInput(ns("selectArmIntra"), label = "If several arms (optional) please select the column that identifies the observation's arm",
                       choices = c(Choose = "", NULL),
                       options = list(placeholder = 'Please select a column name below')
        )
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 2 & input['%s'] != '' & !output.armisfactor2 & output.warningarm2isfactor != null",ns("selectModel"),ns("selectArmIntra")) ,#"input.selectModel == 2 & input.selectArmIntra != '' & !output.armisfactor2 & output.warningarm2isfactor != null",
        verbatimTextOutput(ns("warningarm2isfactor"))
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 2 & input['%s'] != '' & output.arm2isfactor",ns("selectModel"),ns("selectArmIntra")) ,#"input.selectModel == 2 & input.selectArmIntra != '' & output.arm2isfactor",
        selectizeInput(ns("selectRefArmIntra"), label = "Select the arm to analyze",
                       choices =c(Choose = "", NULL))
      ),
      
      h3("Denominator degrees of freedom approximations"),
      radioButtons(ns("ddf"), NULL,
                   choices = c("Satterthwaite",
                               "Kenward-Roger",
                               "Between-Within"),
                   selected = "Satterthwaite"),
      
      tags$hr(),
      h3("Run analysis"),
      mod_modelfit_ui("modelfit_ui_1"),
      
      h3(),
      tags$hr(),
      h3("Example data"),
      fluidRow(
        actionButton(ns("loadExample"), label = "load example data")
      ),
      fluidRow(
        downloadButton(ns("downloadExData"), label = "download example data")
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_settings_pan
#' @export
#' @keywords internal
    
mod_settings_pan_server <- function(input, output, session,data,parent){
  ns <- session$ns
  #callModule(module = mod_modelfit_server, id = "modelfit_ui_1",data = data,parent = parent,parentModule = session)

  # example data
  output$downloadExData <- downloadHandler(   #Fait appel à une lib externe donc pas besoin de tester
    filename = "exampleICSdata.txt",
    content = function(file) {
      utils::write.table(vici::ICS_ex, file, row.names = FALSE, sep="\t", quote = FALSE)
    }
  )
  
  observeEvent(input$loadExample,{
    #cat("observe loadExample", "\n")

    data$df <<- vici::ICS_ex
    
    clean_output(output)

    parent$output$table2render <- DT::renderDataTable(data$df,
                                               options = list(pageLength = 10, lengthMenu = list(c(5, 10, -1), c('5', '10', 'All')))
    )
    
    #Ensemble setter
    output$mod <- reactive(NULL)
    output$mod_display <- reactive(FALSE)
    updateRadioButtons(session, inputId = "sep", selected = "\t")
    updateCheckboxInput(session, inputId = "header", value = TRUE)

    observeEvent(input$selectModel, {
      cat("observe selectModel", "\n")
      if (input$selectModel==1){
        # updateSelectInput(session, "selectModel", selected = 1)
        available_vars_init <- colnames(data$df)
        updateSelectizeInput(session, "selectSubject",
                             selected = 'Subject',
                             choices = c('', available_vars_init),
                             options = list(placeholder = 'Please select a variable below')
        )
        updateSelectizeInput(session, "selectResponse",
                             selected = c('Response1', 'Response2'),
                             choices = c('', available_vars_init),
                             options = list(placeholder = 'Please select a variable below')
        )
        updateSelectizeInput(session, "selectStim",
                             selected = 'StimulationPool',
                             choices = c('', available_vars_init),
                             options = list(placeholder = 'Please select a variable below')
        )
        updateSelectizeInput(session, "selectRefStim",
                             selected = 'NS',
                             choices = c(levels(data$df$StimulationPool))
        )
        updateSelectizeInput(session, "selectArmInter",
                             selected = 'Arm',
                             choices = c('', available_vars_init),
                             options = list(placeholder = 'Please select a variable below')
        )
        updateSelectizeInput(session, "selectRefArmInter",
                             selected = 'Placebo',
                             choices = c(levels(data$df$Arm))
        )
        updateSelectizeInput(session, "selectTimeInter",
                             selected = 'TimePoint',
                             choices = c('', available_vars_init),
                             options = list(placeholder = 'Please select a variable below')
        )
        updateSelectizeInput(session, "selectRefTimeInter",
                             selected = 'D1',
                             choices = c(levels(data$df$TimePoint))
        )
        updateTabsetPanel(parent, "inTabset", selected = "dataTab")
      }else if (input$selectModel==2){
        # updateSelectInput(session, "selectModel", selected = 2)
        available_vars_init <- colnames(data$df)
        updateSelectizeInput(session, "selectSubject",
                             selected = 'Subject',
                             choices = c('', available_vars_init),
                             options = list(placeholder = 'Please select a variable below')
        )
        updateSelectizeInput(session, "selectResponse",
                             selected = c('Response1', 'Response2'),
                             choices = c('', available_vars_init),
                             options = list(placeholder = 'Please select a variable below')
        )
        updateSelectizeInput(session, "selectStim",
                             selected = 'StimulationPool',
                             choices = c('', available_vars_init),
                             options = list(placeholder = 'Please select a variable below')
        )
        updateSelectizeInput(session, "selectRefStim",
                             selected = 'NS',
                             choices = c(levels(data$df$StimulationPool))
        )
        updateSelectizeInput(session, "selectTimeIntra",
                             selected = 'TimePoint',
                             choices = c('', available_vars_init),
                             options = list(placeholder = 'Please select a variable below')
        )
        updateSelectizeInput(session, "selectRefTimeIntra",
                             selected = 'D0',
                             choices = c(levels(data$df$TimePoint))
        )
        updateSelectizeInput(session, "selectArmIntra",
                             selected = 'Arm',
                             choices = c('', available_vars_init),
                             options = list(placeholder = 'Please select a variable below')
        )
        updateSelectizeInput(session, "selectRefArmIntra",
                             selected = 'A2',
                             choices = c(levels(data$df$Arm))
        )
        updateTabsetPanel(parent, "inTabset", selected = "dataTab")

      }
    })
  })
  
  
  observeEvent({input$datafile; input$header; input$sep}, {
    cat("observe datainput", "\n")
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
      
      #Setters 
      clean_output(parent$output)
      parent$output$mod <- reactive(NULL)
      parent$output$mod_display <- reactive(FALSE)
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
    updateSelectizeInput(session, "selectArmInter",
                         selected = '',
                         choices = c('', available_vars_init),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectArmIntra",
                         selected = '',
                         choices = c('', available_vars_init),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectTimeIntra",
                         selected = '',
                         choices = c('', available_vars_init),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectTimeInter",
                         selected = '',
                         choices = c('', available_vars_init),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectRefTimeIntra",
                         selected = ''
    )
    updateSelectizeInput(session, "selectRefTimeInter",
                         selected = ''
    )
    updateSelectizeInput(session, "selectRefArmInter",
                         selected = ''
    )
    updateSelectizeInput(session, "selectRefArmIntra",
                         selected = ''
    )
    updateSelectizeInput(session, "selectRefStim",
                         selected = ''
    )
    updateTabsetPanel(parent, "inTabset", selected = "dataTab")
  })
  
  parent$output$table2render <- DT::renderDataTable(
    {
      req(input$datafile)
      data$df
    },
    options = list(pageLength = 10, lengthMenu = list(c(5, 10, -1), c('5', '10', 'All')))
  )
  
  
  # update available variables for selection ----
  # observeEvent available_vars ----
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
    updateSelectizeInput(session, "selectArmInter",
                         choices = c(input$selectArmInter, data$available_vars, intToUtf8(160)),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectArmIntra",
                         choices = c(input$selectArmIntra, data$available_vars, intToUtf8(160)),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectTimeIntra",
                         choices = c(input$selectTimeIntra, data$available_vars, intToUtf8(160)),
                         options = list(placeholder = 'Please select a variable below')
    )
    updateSelectizeInput(session, "selectTimeInter",
                         choices = c(input$selectTimeInter, data$available_vars, intToUtf8(160)),
                         options = list(placeholder = 'Please select a variable below')
    )}
  )
  
  observeEvent(input$selectSubject, {
    if (input$selectSubject != ''){
      data$available_vars <-  update_vars(input, possibilities = colnames(data$df)) #A tester
    }
    clean_output(output)
  })
  
  observeEvent(input$selectStim, {
    #cat("observe selectStim", "\n")
    if (input$selectStim != ''){
      data$available_vars <-  update_vars(input, possibilities = colnames(data$df))
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
    }
    clean_output(output)
  })
  
  observeEvent(input$selectResponse, {
    #cat("observe selectResp", "\n")
    if (length(input$selectResponse) >= 1){
      if (input$selectResponse[1] != ''){
        
        data$available_vars <- update_vars(input, possibilities = colnames(data$df))
        
        clean_output(output)
      }
    }
  })
  
  observeEvent(input$selectArmInter, {
    #cat("observe selectArmInter", "\n")
    if (input$selectArmInter != ''){
      
      data$available_vars <- update_vars(input, possibilities = colnames(data$df))
      
      
      if (input$selectArmInter %in% colnames(data$df)){
        selected_arm_var <- data$df[, input$selectArmInter]
        if(is.factor(selected_arm_var)){
          output$armisfactor <- reactive(TRUE)
          possible_arms <- levels(selected_arm_var)
          output$warningarmisfactor <- reactive(NULL)
          data$fact_arm_OK <- TRUE
          # if(length(possible_arms) > 2){
          #   output$armisfactor <- reactive(FALSE)
          #   output$warningarmisfactor <- reactive(paste0("Error: '", input$selectArmInter, "' has more than 2 levels\n This is not implemented yet"))
          #   data$fact_arm_OK <- FALSE
          # }
        }else{
          output$armisfactor <- reactive(FALSE)
          output$warningarmisfactor <- reactive(paste0("WARNING: '", input$selectArmInter,
                                                       "' is not a factor"))
          data$fact_arm_OK <- FALSE
          possible_arms <- paste0("Error: '", input$selectArmInter,
                                  "' is not a factor\nPlease select a different variable")
        }
        updateSelectizeInput(session, "selectRefArmInter",
                             choices = c(possible_arms[1], possible_arms),
                             selected = ifelse(is.null(input$selectRefArmInter) | (length(input$selectRefArmInter)>0 && input$selectRefArmInter==''),
                                               possible_arms[1], input$selectRefArmInter)
        )
      }else if(input$selectArmInter != intToUtf8(160)){
        output$armisfactor <- reactive(FALSE)
        output$warningarmisfactor <- reactive(paste0("WARNING: '", input$selectArmInter,
                                                     "' is not a column in the input data"))
        data$fact_arm_OK <- FALSE
      }else{
        output$armisfactor <- reactive(FALSE)
        output$warningarmisfactor <- reactive(NULL)
        data$fact_arm_OK <- FALSE
        
        data$available_vars <-  update_vars(input, possibilities = colnames(data$df)) #A tester
        
      }
    }else{
      
      data$available_vars <-  update_vars(input, possibilities = colnames(data$df))
      
    }
    clean_output(output)
  })
  
  
  observeEvent(input$selectArmIntra, {
    #cat("observe selectArmIntra", "\n")
    if (input$selectArmIntra != ''){
      
      data$available_vars <-  update_vars(input, possibilities = colnames(data$df))
      
      
      if (input$selectArmIntra %in% colnames(data$df)){
        selected_arm2_var <- data$df[, input$selectArmIntra]
        if(is.factor(selected_arm2_var)){
          output$arm2isfactor <- reactive(TRUE)
          possible_arm2s <- levels(selected_arm2_var)
          output$warningarm2isfactor <- reactive(NULL)
          data$fact_arm2_OK <- TRUE
          # if(length(possible_arm2s) > 2){
          #   output$arm2isfactor <- reactive(FALSE)
          #   output$warningarm2isfactor <- reactive(paste0("Error: '", input$selectArmIntra, "' has more than 2 levels\n This is not implemented yet"))
          #   data$fact_arm2_OK <- FALSE
          # }
        }else{
          output$arm2isfactor <- reactive(FALSE)
          output$warningarm2isfactor <- reactive(paste0("WARNING: '", input$selectArmIntra, #paste concatène chaine caractère 
                                                        "' is not a factor"))
          data$fact_arm2_OK <- FALSE
          possible_arm2s <- paste0("Error: '", input$selectArmIntra,
                                   "' is not a factor\nPlease select a different variable")
        }
        updateSelectizeInput(session, "selectRefArmIntra",
                             choices = c(possible_arm2s[1], possible_arm2s),
                             selected = ifelse(is.null(input$selectRefArmIntra) | (length(input$selectRefArmIntra)>0 && input$selectRefArmIntra==''),
                                               possible_arm2s[1], input$selectRefArmIntra)
        )
      }else if(input$selectArmIntra != intToUtf8(160)){
        output$arm2isfactor <- reactive(FALSE)
        output$warningarm2isfactor <- reactive(paste0("WARNING: '", input$selectArmIntra,
                                                      "' is not a column in the input data"))
        data$fact_arm2_OK <- FALSE
      }else{
        output$arm2isfactor <- reactive(FALSE)
        output$warningarm2isfactor <- reactive(NULL)
        data$fact_arm2_OK <- FALSE
        data$available_vars <-  update_vars(input, possibilities = colnames(data$df))
      }
    }else{
      data$available_vars <-  update_vars(input, possibilities = colnames(data$df))
    }
    clean_output(output)
  })
  
  observeEvent(input$selectModel, {
    if(!is.null(data$available_vars)){
      updateSelectizeInput(session, "selectArmInter",
                           choices = union(c('', data$available_vars),
                                           union(union(input$selectArmInter, input$selectArmIntra), input$selectTimeInter))
      )
      updateSelectizeInput(session, "selectArmIntra",
                           choices = union(c('', data$available_vars),
                                           union(union(input$selectArmInter, input$selectArmIntra), input$selectTimeIntra))
      )
      updateSelectizeInput(session, "selectTimeIntra",
                           choices = union(c('', data$available_vars),
                                           union(union(input$selectArmInter, input$selectArmIntra), input$selectTimeIntra))
      )
      updateSelectizeInput(session, "selectTimeInter",
                           choices = union(c('', data$available_vars),
                                           union(union(input$selectArmInter, input$selectArmIntra), input$selectTimeInter))
      )
      updateSelectizeInput(session, "selectRefTimeIntra",
                           selected = ''
      )
      updateSelectizeInput(session, "selectRefTimeInter",
                           selected = ''
      )
      updateSelectizeInput(session, "selectRefArmInter",
                           selected = ''
      )
      updateSelectizeInput(session, "selectRefArmIntra",
                           selected = ''
      )
    }
    clean_output(output)
  }
  )
  
  # observe time ----
  observeEvent(input$selectTimeIntra, {
    #cat("observe selectTimeIntra", "\n")
    if (input$selectTimeIntra != ''){
      data$available_vars <-  update_vars(input, possibilities = colnames(data$df))
      if(input$selectTimeIntra %in% colnames(data$df)){
        data$df[, input$selectTimeIntra] <- as.factor(as.character(data$df[, input$selectTimeIntra]))
        selected_time_var <- data$df[, input$selectTimeIntra]
        output$timeisfactor <- reactive(TRUE)
        possible_times <- levels(selected_time_var)
        output$warnintimeisfactor <- reactive(NULL)
        data$fact_time_OK <- TRUE
        
        updateSelectizeInput(session, "selectRefTimeIntra",
                             choices = c(possible_times[1], possible_times),
                             selected = ifelse(is.null(input$selectRefTimeIntra) | (length(input$selectRefTimeIntra)>0 && input$selectRefTimeIntra==''),
                                               possible_times[1], input$selectRefTimeIntra)
        )
      }else if(input$selectTimeIntra != intToUtf8(160)){
        output$timeisfactor <- reactive(FALSE)
        output$warnintimeisfactor <- reactive(paste0("WARNING: '", input$selectTimeIntra,
                                                     "' is not a column in the input data"))
        data$fact_time_OK <- FALSE
      }else{
        output$timeisfactor <- reactive(FALSE)
        output$warnintimeisfactor <- reactive(NULL)
        data$fact_time_OK <- FALSE
        data$available_vars <-  update_vars(input, possibilities = colnames(data$df))
      }
    }else{
      data$available_vars <-  update_vars(input, possibilities = colnames(data$df))
    }
    clean_output(output)
  })
  
  # observe time ----
  observeEvent(input$selectTimeInter, {
    #cat("observe selectTimeInter", "\n")
    if (input$selectTimeInter != ''){
      data$available_vars <-  update_vars(input, possibilities = colnames(data$df))
      if(input$selectTimeInter %in% colnames(data$df)){
        data$df[, input$selectTimeInter] <- as.factor(as.character(data$df[, input$selectTimeInter]))
        selected_time2_var <- data$df[, input$selectTimeInter]
        output$time2isfactor <- reactive(TRUE)
        possible_times2 <- levels(selected_time2_var)
        output$warnintime2isfactor <- reactive(NULL)
        data$fact_time2_OK <- TRUE
        
        updateSelectizeInput(session, "selectRefTimeInter",
                             choices = c(possible_times2[1], possible_times2),
                             selected = ifelse(is.null(input$selectRefTimeInter) | (length(input$selectRefTimeInter)>0 && input$selectRefTimeInter==''),
                                               possible_times2[1], input$selectRefTimeInter)
        )
      }else if(input$selectTimeInter != intToUtf8(160)){
        output$time2isfactor <- reactive(FALSE)
        output$warnintime2isfactor <- reactive(paste0("WARNING: '", input$selectTimeInter,
                                                      "' is not a column in the input data"))
        data$fact_time2_OK <- FALSE
      }else{
        output$time2isfactor <- reactive(FALSE)
        output$warnintime2isfactor <- reactive(NULL)
        data$fact_time2_OK <- FALSE
        data$available_vars <-  update_vars(input, possibilities = colnames(data$df))
      }
    }else{
      data$available_vars <-  update_vars(input, possibilities = colnames(data$df))
    }
    clean_output(output)
  })
  
  
  observeEvent(input$ddf, {

    clean_output(parent$output)
  })
  
  
  observeEvent({input$selectRefArmInter; input$selectRefArmIntra; input$selectRefStim; input$selectRefTimeIntra; input$selectRefTimeInter}, {
    #cat("observe selectRefs", "\n")
    clean_output(output) # a tester
  })
  
  #callModule(module = mod_modelfit_server, id = "modelfit_ui_1",data = data,parent = parent,parentModule = session)
  return(input)
}
    
## To be copied in the UI
# mod_settings_pan_ui("settings_pan_ui_1")
    
## To be copied in the server
# callModule(mod_settings_pan_server, "settings_pan_ui_1")
 
