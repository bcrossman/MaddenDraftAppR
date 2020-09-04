#' draft_board UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_draft_board_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("a table"),
    DT::DTOutput(ns("drafttable"))
  )
}
    
#' draft_board Server Function
#'
#' @noRd 
mod_draft_board_server <- function(input, output, session){
  ns <- session$ns
  output$drafttable <- DT::renderDataTable({
    DT::datatable(draft_data %>% dplyr::select(firstName, lastName))
  })
}
    
## To be copied in the UI
# mod_draft_board_ui("draft_board_ui_1")
    
## To be copied in the server
# callModule(mod_draft_board_server, "draft_board_ui_1")
 
