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
    fluidPage(
      sidebarLayout(
        sidebarPanel = sidebarPanel(width = 2,style="overflow-y:scroll; max-height:800px; position:relative;",
                                    submitButton("Update View", icon("refresh")),
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
                                    )
        ),
        mainPanel = mainPanel(
          tabsetPanel(
            tabPanel(title = "Individual Draft",
                     DT::DTOutput(ns("drafttable"))
                     ),
            tabPanel(title = "Team Draft", DT::DTOutput(ns("drafttableteam")))
          )
        )
      )
    )
  )
}

#' draft_board Server Function
#'
#' @noRd 
mod_draft_board_server <- function(input, output, session){
  ns <- session$ns
  
  player_data_slim_1 <- 
    draft_data %>% 
    dplyr::filter(draftRound<56) %>%
    dplyr::mutate(salary_per_year = contractSalary/contractLength) %>% 
    dplyr::mutate(id = as.character(id))
  
  total_salary <- sum(player_data_slim_1$salary_per_year, na.rm = T)
  
  player_data_value <- 
    player_data_slim_1 %>% 
    dplyr::filter(draftRound<56) %>%
    dplyr::mutate(z_score = standard_mutate(playerBestOvr)) %>% 
    dplyr::group_by(position) %>% 
    dplyr::mutate(replacement = quantile(z_score, probs = .05)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(zsar = z_score - replacement) %>% 
    dplyr::mutate(salary_per_year = contractSalary/contractLength) %>% 
    dplyr::left_join(df_adj %>%dplyr::select(term, adjustment), by = c("position"  = "term")) %>% 
    dplyr::mutate(zsar_adj = zsar *adjustment) %>% 
    dplyr::mutate(annual_value = round((total_salary/sum(zsar_adj, na.rm = T))*zsar_adj),0) %>% 
    dplyr::mutate(annual_excess_value = annual_value - salary_per_year) %>% 
    dplyr::select(id, team, firstName, lastName, position, age, playerBestOvr, devTrait, 
                  z_score, replacement, zsar, adjustment, zsar_adj,
                  salary_per_year, contractYearsLeft,
                  annual_value, annual_excess_value) %>% 
    dplyr::mutate(adj_contract_years_left = pmin(pmax(29-age,1), contractYearsLeft)) %>% 
    dplyr::mutate(total_value_contract = adj_contract_years_left * annual_excess_value) %>% 
    dplyr::arrange(dplyr::desc(total_value_contract))
  
  ##Expected Draft position
  draft_position <- 
    draft_data %>% 
    dplyr::filter(draftRound<56) %>%
    dplyr::select(id, draftPick, draftRound) %>% 
    dplyr::mutate(expect_draft_pos = (draftRound-2)*32+(draftPick - 1),
                  id = as.character(id)) %>% 
    dplyr::select(-draftPick, -draftRound)
  
  
  player_data_value_final <- reactive({
    WR_chose_attr <-    input$attributes_target_WR
    QB_chose_attr <-    input$attributes_target_QB
    C_chose_attr <-     input$attributes_target_C
    HB_chose_attr <-    input$attributes_target_HB
    FB_chose_attr <-    input$attributes_target_FB
    CB_chose_attr <-    input$attributes_target_CB
    RE_chose_attr <-    input$attributes_target_RE
    RG_chose_attr <-    input$attributes_target_RG
    RT_chose_attr <-    input$attributes_target_RT
    LE_chose_attr <-    input$attributes_target_LE
    LG_chose_attr <-    input$attributes_target_LG
    LT_chose_attr <-    input$attributes_target_LT
    DT_chose_attr <-    input$attributes_target_DT
    LOLB_chose_attr <-  input$attributes_target_LOLB
    ROLB_chose_attr <-  input$attributes_target_ROLB
    SS_chose_attr <-    input$attributes_target_SS
    FS_chose_attr <-    input$attributes_target_FS
    TE_chose_attr <-    input$attributes_target_TE
    MLB_chose_attr <-   input$attributes_target_MLB
    P_chose_attr <-     input$attributes_target_P
    K_chose_attr <-     input$attributes_target_K
    
    
    # WR_chose_attr <- c("playerBestOvr","speedRating","routeRunShortRating","runBlockFinesseRating","runBlockPowerRating" )
    # QB_chose_attr <- c("playerBestOvr","speedRating")
    # HB_chose_attr <- c("playerBestOvr")
    # FB_chose_attr <- c("playerBestOvr", "elusiveRating", "jukeMoveRating")
    # CB_chose_attr <- c("playerBestOvr","speedRating")
    # RE_chose_attr <- c("playerBestOvr","speedRating")
    # RG_chose_attr <- c("playerBestOvr","speedRating")
    # RT_chose_attr <- c("playerBestOvr","speedRating")
    # LE_chose_attr <- c("playerBestOvr","speedRating")
    # LG_chose_attr <- c("playerBestOvr","speedRating")
    # LT_chose_attr <- c("playerBestOvr","speedRating")
    # DT_chose_attr <- c("playerBestOvr","speedRating")
    # LOLB_chose_attr <- c("playerBestOvr","speedRating")
    # ROLB_chose_attr <- c("playerBestOvr","speedRating")
    # SS_chose_attr <- c("playerBestOvr","speedRating")
    # FS_chose_attr <- c("playerBestOvr","speedRating")
    # TE_chose_attr <- c("playerBestOvr","speedRating")
    # MLB_chose_attr <- c("playerBestOvr","speedRating")
    # C_chose_attr <- c("playerBestOvr","speedRating")
    # P_chose_attr <- c("playerBestOvr","speedRating")
    # K_chose_attr <- c("playerBestOvr","speedRating")
    
    attr_obj <- tibble::lst(
      WR_chose_attr ,  
      QB_chose_attr ,  
      C_chose_attr ,   
      HB_chose_attr ,  
      FB_chose_attr ,  
      CB_chose_attr ,  
      RE_chose_attr ,  
      RG_chose_attr ,  
      RT_chose_attr ,  
      LE_chose_attr ,  
      LG_chose_attr ,  
      LT_chose_attr ,  
      DT_chose_attr ,    
      LOLB_chose_attr ,  
      ROLB_chose_attr ,
      SS_chose_attr ,    
      FS_chose_attr ,    
      TE_chose_attr ,    
      MLB_chose_attr , 
      P_chose_attr ,     
      K_chose_attr,
    )
    
    df_attr <- plyr::ldply(attr_obj, rbind)
    df_attr <- tidyr::gather(data = df_attr, key = key, value = value, na.rm = T, -.id)
    
    df_attr <- 
      df_attr %>% 
      dplyr::select(-key) %>% 
      dplyr::rename(key = .id)
    
    df_attr <- df_attr %>% dplyr::mutate(key = gsub(pattern = "_chose_attr",replacement = "", x = .[["key"]]))
    
    df_attr$use = 1
    
    df_attr <- 
      df_attr %>%
      dplyr::rename(position = key,
                    attribute = value)
    
    df_initial <- 
      player_data_slim_1 %>% 
      dplyr::filter(draftRound<56) %>%
      dplyr::ungroup() %>% 
      # group_by(position) %>% 
      dplyr::mutate_if(is.numeric, .funs = standard_mutate) %>% 
      tidyr::gather(key = "attribute", "value", -id, -team, -firstName, -lastName, -position) %>% 
      dplyr::left_join(df_attr) %>% 
      dplyr::filter(use == 1) %>% 
      dplyr::mutate(id = as.character(id)) %>% 
      dplyr::group_by(id, team, firstName, lastName, position) %>% 
      dplyr::mutate(value = round(as.numeric(value)),0) %>% 
      dplyr::summarise(value = round(mean(value, na.rm = T)),0) %>% 
      dplyr::arrange(dplyr::desc(value))
    
    player_data_value_2 <- 
      df_initial %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(z_score = standard_mutate(value)) %>% 
      dplyr::group_by(position) %>% 
      dplyr::mutate(replacement = quantile(z_score, probs = .10)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(zsar = z_score - replacement) %>% 
      dplyr::left_join(df_adj %>%dplyr::select(term, adjustment), by = c("position"  = "term")) %>% 
      dplyr::mutate(zsar_adj = zsar *adjustment) %>% 
      dplyr::mutate(annual_value_pick = round((total_salary/sum(zsar_adj, na.rm = T))*zsar_adj),0) %>% 
      dplyr::select(id:position, annual_value_pick)
    
    player_data_value_final <- 
      player_data_value %>% dplyr::left_join(player_data_value_2) %>% 
      dplyr::mutate(annual_excess_value_pick = round(annual_value_pick - salary_per_year,0), 
                    total_value_contract_pick = adj_contract_years_left * annual_excess_value_pick) %>% 
      dplyr::arrange(dplyr::desc(total_value_contract_pick)) %>% 
      dplyr::left_join(draft_position) %>% 
      dplyr::mutate(actual_value_pos = rank(dplyr::desc(total_value_contract_pick))) %>% 
      dplyr::mutate(value_pick = -(actual_value_pos-expect_draft_pos))
    
    return(player_data_value_final)
  })
  
  output$drafttable <- DT::renderDataTable({
    DT::datatable(player_data_value_final() %>% dplyr::select(-id), extensions = c('Buttons','FixedColumns'),
                  filter = "top",
                  options = list(
                    scrollX = TRUE,
                    pageLength = -1, 
                    scrollY = "700px",
                    fixedColumns = list(leftColumns = 4), 
                    dom = 'Bfrti',
                    buttons = list(
                      list(extend = 'collection',
                           buttons = c('excel', 'csv'),
                           text = 'DOWNLOAD DATA')
                    )
                  )
    ) 
  })
  
  output$drafttableteam <- DT::renderDataTable({
    
    draft_team <- 
      player_data_value_final() %>% 
      dplyr::left_join(required_depth) %>% 
      dplyr::arrange(desc(total_value_contract_pick)) %>% 
      dplyr::group_by(team, position) %>% 
      dplyr::mutate(rank = rank(dplyr::desc(annual_value_pick), ties.method = "first")) %>% 
      dplyr::filter(rank <= num) %>%
      dplyr::summarise(value = round(mean(total_value_contract_pick),0)) %>%
      tidyr::spread(key = position, value = value, fill = -1000000) %>%
      dplyr::mutate(Total = C+ CB+ DT+ FS+ HB+ K+ LE+ LG+ LOLB+ LT+ MLB+ P+ QB+ RE+ RG+ ROLB+ RT+ SS+ TE+ WR) %>%
      dplyr::arrange(dplyr::desc(Total))
      
    DT::datatable(draft_team, 
                  extensions = c('Buttons','FixedColumns'),
                  filter = "top",
                  options = list(
                    scrollX = TRUE,
                    pageLength = -1, 
                    scrollY = "700px",
                    fixedColumns = list(leftColumns = 1), 
                    dom = 'Bfrti',
                    buttons = list(
                      list(extend = 'collection',
                           buttons = c('excel', 'csv'),
                           text = 'DOWNLOAD DATA')
                    )
                  )
    ) 
  })
}

## To be copied in the UI
# mod_draft_board_ui("draft_board_ui_1")

## To be copied in the server
# callModule(mod_draft_board_server, "draft_board_ui_1")

