#' Launch VICI Shiny App
#'
#'@param host Default is "127.0.0.1", see \link[shiny]{runApp} for details.
#'@param port Default is 3838, see \link[shiny]{runApp} for details.
#'@param ... additional arguments to be passed to the \link[shiny]{runApp} function.
#'
#'@examples
#'if(interactive()){
#' vici::run_app()
#'}
#'
#' @export
#' @importFrom shiny runApp
run_app <- function(host="127.0.0.1", port=3838, ...) {
  shiny::runApp(system.file("app", package = "vici"), port=port, host=host, ...)
}

