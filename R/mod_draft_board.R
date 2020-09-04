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
    column(3,
      selectInput(inputId = ns("attributes_target_QB"),
                  label = "QB Desired Attributes:",
                  choices = names(draft_data %>% dplyr::select(-teamId,-firstName, -team,  -lastName,-position)),
                  # selected = c("playerBestOvr", "youth", "devTrait", "throwAccDeepRating", 
                  #              "throwAccMidRating", "throwAccRating", 
                  #              "throwAccShortRating", "throwPowerRating", 
                  #              "throwUnderPressureRating", "tightSpiralTrait"),
                  selected = c("playerBestOvr", "youth", "devTrait", "throwAccDeepRating", 
                               "throwOnRunRating","throwPowerRating", "playActionRating",
                               "throwUnderPressureRating", "tightSpiralTrait"),
                  multiple = TRUE
      ),
      
      selectInput(inputId = ns("attributes_target_WR"),
                  label = "WR Desired Attributes:",
                  choices = names(draft_data %>% dplyr::select(-teamId,-firstName, -team,  -lastName,-position)),
                  selected = c("playerBestOvr", "youth", "devTrait", "speedRating", 
                               "accelRating", "releaseRating", "catchRating"),
                  multiple = TRUE
      ),
      
      selectInput(inputId = ns("attributes_target_TE"),
                  label = "TE Desired Attributes:",
                  choices = names(draft_data %>% dplyr::select(-teamId,-firstName, -team,  -lastName,-position)),
                  selected = c("playerBestOvr", "youth", "devTrait", "catchRating", 
                               "passBlockRating", "awareRating", "playRecRating"),
                  multiple = TRUE
      ),
      
      selectInput(inputId = ns("attributes_target_HB"),
                  label = "HB Desired Attributes:",
                  choices = names(draft_data %>% dplyr::select(-teamId,-firstName, -team,  -lastName,-position)),
                  selected = c("playerBestOvr", "youth", "devTrait", "catchRating", 
                               "passBlockRating", "awareRating", "playRecRating"),
                  multiple = TRUE
      ),
      
      selectInput(inputId = ns("attributes_target_FB"),
                  label = "FB Desired Attributes:",
                  choices = names(draft_data %>% dplyr::select(-teamId,-firstName, -team,  -lastName,-position)),
                  selected = c("playerBestOvr"),
                  multiple = TRUE
      ),
      
      selectInput(inputId = ns("attributes_target_LE"),
                  label = "LE Desired Attributes:",
                  choices = names(draft_data %>% dplyr::select(-teamId,-firstName, -team,  -lastName,-position)),
                  selected = c("playerBestOvr", "youth", "devTrait", "blockShedRating", 
                               "awareRating", "playRecRating", 
                               "tackleRating", "pursuitRating"),
                  multiple = TRUE
      ),
      
      selectInput(inputId = ns("attributes_target_RE"),
                  label = "RE Desired Attributes:",
                  choices = names(draft_data %>% dplyr::select(-teamId,-firstName, -team,  -lastName,-position)),
                  selected = c("playerBestOvr", "youth", "devTrait", "blockShedRating", 
                               "awareRating", "playRecRating", 
                               "tackleRating", "pursuitRating"),
                  multiple = TRUE
      ),
      
      selectInput(inputId = ns("attributes_target_DT"),
                  label = "DT Desired Attributes:",
                  choices = names(draft_data %>% dplyr::select(-teamId,-firstName, -team,  -lastName,-position)),
                  selected = c("playerBestOvr", "youth", "devTrait", "blockShedRating", 
                               "awareRating", "playRecRating", 
                               "tackleRating", "pursuitRating"),
                  multiple = TRUE
      ),
      
      selectInput(inputId = ns("attributes_target_C"),
                  label = "C Desired Attributes:",
                  choices = names(draft_data %>% dplyr::select(-teamId,-firstName, -team,  -lastName,-position)),
                  selected = c("playerBestOvr", "youth", "devTrait",  "awareRating", 
                               "playRecRating", "passBlockRating",
                               "passBlockFinesseRating", "passBlockPowerRating"),
                  multiple = TRUE
      ),
      
      selectInput(inputId = ns("attributes_target_RG"),
                  label = "RG Desired Attributes:",
                  choices = names(draft_data %>% dplyr::select(-teamId,-firstName, -team,  -lastName,-position)),
                  selected = c("playerBestOvr", "youth", "devTrait",  "awareRating", 
                               "playRecRating", "passBlockRating",
                               "passBlockFinesseRating", "passBlockPowerRating"),
                  multiple = TRUE
      ),
      
      selectInput(inputId = ns("attributes_target_LG"),
                  label = "LG Desired Attributes:",
                  choices = names(draft_data %>% dplyr::select(-teamId,-firstName, -team,  -lastName,-position)),
                  selected = c("playerBestOvr", "youth", "devTrait",  "awareRating", 
                               "playRecRating", "passBlockRating",
                               "passBlockFinesseRating", "passBlockPowerRating"),
                  multiple = TRUE
      ),
      
      selectInput(inputId = ns("attributes_target_RT"),
                  label = "RT Desired Attributes:",
                  choices = names(draft_data %>% dplyr::select(-teamId,-firstName, -team,  -lastName,-position)),
                  selected = c("playerBestOvr", "youth", "devTrait",  "awareRating", 
                               "playRecRating", "passBlockRating",
                               "passBlockFinesseRating", "passBlockPowerRating"),
                  multiple = TRUE
      ),
      
      selectInput(inputId = ns("attributes_target_LT"),
                  label = "LT Desired Attributes:",
                  choices = names(draft_data %>% dplyr::select(-teamId,-firstName, -team,  -lastName,-position)),
                  selected = c("playerBestOvr", "youth", "devTrait",  "awareRating", 
                               "playRecRating", "passBlockRating",
                               "passBlockFinesseRating", "passBlockPowerRating"),
                  multiple = TRUE
      ),
      
      selectInput(inputId = ns("attributes_target_LOLB"),
                  label = "LOLB Desired Attributes:",
                  choices = names(draft_data %>% dplyr::select(-teamId,-firstName, -team,  -lastName,-position)),
                  selected = c("playerBestOvr", "youth", "devTrait", "awareRating", "playRecRating", 
                               "speedRating", "accelRating",
                               "tackleRating", "pursuitRating"),
                  multiple = TRUE
      ),
      
      selectInput(inputId = ns("attributes_target_ROLB"),
                  label = "ROLB Desired Attributes:",
                  choices = names(draft_data %>% dplyr::select(-teamId,-firstName, -team,  -lastName,-position)),
                  selected = c("playerBestOvr", "youth", "devTrait", "awareRating", 
                               "playRecRating", "speedRating", "accelRating", 
                               "tackleRating", "pursuitRating"),
                  multiple = TRUE
      ),
      
      selectInput(inputId = ns("attributes_target_MLB"),
                  label = "MLB Desired Attributes:",
                  choices = names(draft_data %>% dplyr::select(-teamId,-firstName, -team,  -lastName,-position)),
                  selected = c("playerBestOvr", "youth", "devTrait", "awareRating", "playRecRating", 
                               "speedRating", "accelRating", 
                               "tackleRating", "pursuitRating", "zoneCoverRating", "changeOfDirectionRating"),
                  multiple = TRUE
      ),
      
      selectInput(inputId = ns("attributes_target_CB"),
                  label = "CB Desired Attributes:",
                  choices = names(draft_data %>% dplyr::select(-teamId,-firstName, -team,  -lastName,-position)),
                  selected = c("playerBestOvr", "youth", "devTrait", "awareRating", "playRecRating", 
                               "speedRating", "accelRating", "agility",
                               "manCoverRating", "catchRating", "playBallTrait"),
                  multiple = TRUE
      ),
      
      selectInput(inputId = ns("attributes_target_SS"),
                  label = "SS Desired Attributes:",
                  choices = names(draft_data %>% dplyr::select(-teamId,-firstName, -team,  -lastName,-position)),
                  selected = c("playerBestOvr", "youth", "devTrait", "awareRating", "playRecRating", 
                               "speedRating", "accelRating", 
                               "tackleRating", "pursuitRating", "zoneCoverRating", 
                               "catchRating", "playBallTrait", "changeOfDirectionRating"),
                  multiple = TRUE
      ),
      
      selectInput(inputId = ns("attributes_target_FS"),
                  label = "FS Desired Attributes:",
                  choices = names(draft_data %>% dplyr::select(-teamId,-firstName, -team,  -lastName,-position)),
                  selected = c("playerBestOvr", "youth", "devTrait", "awareRating", "playRecRating", 
                               "speedRating", "accelRating", 
                               "tackleRating", "pursuitRating", "zoneCoverRating", "catchRating", "playBallTrait",
                               "changeOfDirectionRating"),
                  multiple = TRUE
      ),
      
      selectInput(inputId = ns("attributes_target_P"),
                  label = "P Desired Attributes:",
                  choices = names(draft_data %>% dplyr::select(-teamId,-firstName, -team,  -lastName,-position)),
                  selected = c("playerBestOvr", "kickAccRating", "kickPowerRating"),
                  multiple = TRUE
      ),
      
      selectInput(inputId = ns("attributes_target_K"),
                  label = "K Desired Attributes:",
                  choices = names(draft_data %>% dplyr::select(-teamId,-firstName, -team,  -lastName,-position)),
                  selected = c("playerBestOvr", "kickAccRating", "kickPowerRating"),
                  multiple = TRUE
      ),
    ),
    column(9,
      DT::DTOutput(ns("drafttable"))
      
    )
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
 
