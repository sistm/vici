#' @import shiny
app_ui <- function() {
  fluidPage(
    titlePanel("VICI: accurate estimation of Vaccine Induced Cellular Immunogenicity with bivariate modeling",
               windowTitle = "VICI v0.2.0"),

    sidebarLayout(
      sidebarPanel(
        # Input: Select a file ----
        h3("Data input"),
        fileInput("datafile", label = "Choose a CSV/TXT file to import",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),

        # Input: Checkbox if file has header ----
        checkboxInput("header", "Header", TRUE),

        # Input: Select separator ----
        radioButtons("sep", "Separator",
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
        selectInput("selectModel", label = "Model choice",
                    choices = list("inter-arm" = 1, "intra-arm" = 2),
                    selected = 1),

        h4("Variable specification"),
        selectizeInput("selectSubject", label = "Select the column that identifies the subject ID",
                       choices = c(Choose = "", NULL),
                       options = list(placeholder = 'Please select a column name below')
        ),

        selectizeInput("selectResponse", label = "Select the column(s) that identify ICS response",
                       choices = c(Choose = "", NULL),
                       options = list(placeholder = 'Please select a column name below'),
                       multiple = TRUE
        ),

        selectizeInput("selectStim", label = "Select the column that identifies the stimulation",
                       choices = c(Choose = "", NULL),
                       options = list(placeholder = 'Please select a column name below')
        ),
        conditionalPanel(
          condition = "input.selectStim != '' & !output.stimisfactor",
          verbatimTextOutput("warningstimisfactor")
        ),
        conditionalPanel(
          condition = "input.selectStim != '' & output.stimisfactor",
          selectizeInput("selectRefStim", label = "Select the value that identifies background samples",
                         choices =c(Choose = "", NULL))
        ),

        selectizeInput("selectArm", label = "Select the column that identifies the arm",
                       choices = c(Choose = "", NULL),
                       options = list(placeholder = 'Please select a column name below')
        ),
        conditionalPanel(
          condition = "input.selectArm != '' & !output.armisfactor",
          verbatimTextOutput("warningarmisfactor")
        ),
        conditionalPanel(
          condition = "input.selectArm != '' & output.armisfactor",
          selectizeInput("selectRefArm", label = "Select the value that identifies the reference arm",
                         choices =c(Choose = "", NULL))
        ),

        tags$hr(),
        h3("Run analysis"),
        actionButton("modelfit", label = "Fit model",
                     class = "btn-primary")
      ),

      mainPanel(
        tabsetPanel(type = "tabs", id="inTabset",
                    tabPanel("Results", value="resTab",
                             # fluidRow(
                             #   column(8,
                             #          withMathJax(),
                             #          h3("Model"),
                             #          uiOutput('mod')),
                             # ),

                             conditionalPanel(
                               condition = "output.heatmap != null | output.res_error != null",
                               tags$hr(),
                               h3("Analysis results"),
                               conditionalPanel(
                                 condition = "output.res_error != null",
                                 verbatimTextOutput("res_error")
                               ),
                               conditionalPanel(
                                 condition = "output.res_lik != null",
                                 #wellPanel(textOutput("res_sentence")),
                                 #h3(""),
                                 wellPanel(
                                   fluidRow(
                                     #column(6,
                                     plotOutput("heatmap"),
                                     h6(""),
                                     downloadButton("downloadHM", label = "Download heatmap [PNG]",
                                                    class = "btn-primary")
                                     #),
                                     # column(6, plotOutput("boxplot"),
                                     #        h6(""),
                                     #        downloadButton("downloadBP", label = "Download boxplot [PNG]",
                                     #                       class = "btn-primary")
                                     # )
                                   )
                                 ),
                                 h2(""),
                                 uiOutput('boxplotsAndTabs')
                               )
                             )
                    ),

                    # Output: Data file ----
                    tabPanel("Data view", value="dataTab",
                             conditionalPanel(
                               #tags$hr(),
                               condition = "output.table2render == null",
                               #h3("Data view")
                               helpText("Please input some data")
                             ),
                             h5(""),
                             DT::dataTableOutput("table2render")
                    ),
                    tabPanel("Additional Information", value="infoTab",
                             conditionalPanel(
                               condition = "output.mod_display",
                               withMathJax(),
                               h4("Statistical model fitted for each ICS response:"),
                               uiOutput('mod')
                               #)
                             ),
                             h5(""),

                             conditionalPanel(
                               condition = "output.res_lik != null",
                               wellPanel(htmlOutput("res_nparam")),
                               h5(""),
                               wellPanel(tableOutput("res_lik")),
                               h5(""),
                               wellPanel(tableOutput("res_var"))
                             )
                    )
        )
      )
    )
  )
}
