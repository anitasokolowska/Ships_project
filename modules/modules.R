
list_ship_types <- fromJSON("./ship_types.json") 
ship_types <- c("", sort(names(list_ship_types)))


#' Ship's type and name selection for map user interface
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
dropdown_mod_ui <- function(id) {
  
  ns <- shiny::NS(id)
  
  tagList(
    div(style = "display: inline-block;vertical-align:top; width: 250px;",
        selectInput(
          inputId = ns("ship_type"),
          label = "Select ship type:",
          choices = ship_types,
          selected = "",
          width = "250px")
    ),
    div(style = "display: inline-block;vertical-align:top; width: 50px;", HTML("<br>")),
    div(style = "display: inline-block;vertical-align:top; width: 250px;",
        selectInput(
          inputId = ns("ship_name"),
          label = "Select ship name:",
          choices = "",
          width = "250px")
    )
  )
}



#' Ship's type and name selection module server-side processing
#'
#' @param input,output,session standard \code{shiny} boilerplate
#'
#' @return list with following components
#' \describe{
#'   \item{ship_type}{reactive character indicating ship's type selection}
#'   \item{ship_name}{reactive character indicating ship's name selection}
#' }
varselect_mod_server <- function(input, output, session) {
# varselect_mod_server <- function(input, output, session, selector) {
  
  # observeEvent(selector(), {
  #   updateSelectInput(session, "ship_name", choices = list_ship_types[[selector()]])
  # })
  
  return(
    list(
      ship_type = reactive({ input$ship_type }),
      ship_name = reactive({ input$ship_name })
    )
  )
}
