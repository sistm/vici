update_vars <- function(input, possibilities){
  setdiff(possibilities, union(union(union(union(union(union(input$selectSubject,
                                                             input$selectStim),
                                                       input$selectResponse),
                                                 input$selectArmInter),
                                           input$selectArmIntra),
                                     input$selectTimeIntra),
                               input$selectTimeInter))
}
