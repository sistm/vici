#' @import shiny
app_ui <- function() {
  fluidPage(
    tags$head(tags$link(rel="shortcut icon", href="www/favicon.ico")),
    titlePanel(div(img(src='www/logo.svg', align="right", height="120"), "VICI: accurate estimation of Vaccine Induced Cellular Immunogenicity with bivariate modeling"),
               windowTitle = "VICI"),
    h6("v0.7.1"),
    h5(),

    shiny::actionLink(inputId='github_code', label="Source code",
                      icon = icon("github"),
                      onclick ="window.open('https://github.com/sistm/vici', '_blank')"),
    shiny::actionLink(inputId='github_code', label="CRAN package",
                      icon = icon("r-project"),
                      onclick ="window.open('https://CRAN.R-project.org/package=vici', '_blank')",
                      style='padding:10px;'),
    h5(),


    sidebarLayout(
      mod_settings_pan_ui("settings_pan_ui_1"),

      mainPanel(tabsetPanel(type = "tabs", id="inTabset",
                    tabPanel("Results", value="resTab",
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
                                 wellPanel(
                                   fluidRow(
                                     plotOutput("heatmap"),
                                     h6(""),
                                     downloadButton("downloadHM", label = "Download heatmap [PNG]",
                                                    class = "btn-primary")
                                   )
                                 ),
                                 h2(""),
                                 uiOutput('boxplotsAndTabs'),
                                 downloadButton("downloadRes", label = "Download Results", class = "btn-primary")
                               )
                             )
                    ),

                    # Output: Data file ----
                    tabPanel("Data view", value="dataTab",
                             conditionalPanel(
                               condition = "output.table2render == null",
                               helpText("Please input some data")
                             ),
                             h5(""),
                             DT::dataTableOutput("table2render")
                    ),
                    tabPanel("Additional Information", value="infoTab",
                             conditionalPanel(
                               condition = "output.mod_display",
                               withMathJax(),
                               h3("Statistical model fitted for each ICS response:"),
                               uiOutput('mod')
                             ),
                             h2(),
                             tags$hr(),

                             conditionalPanel(
                               condition = "output.res_lik != null",
                               h3("Additional estimates:"),
                               wellPanel(htmlOutput("res_nparam")),
                               h5(""),
                               wellPanel(tableOutput("res_lik")),
                               h5(""),
                               wellPanel(h4("Estimated variances"),
                                         tableOutput("res_var"))
                             )
                    )
        )
      )
    )
  )
}
