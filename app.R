library(tidyverse)
library(jsonlite)
library(shiny)
library(shinyjs)
library(semantic.dashboard)
library(leaflet)


# source("./modules/modules.R")

df_data <- read_csv("./data/input_app_data.csv")

list_ship_types <- fromJSON("./data/ship_types.json") 
ship_types <- c("", sort(names(list_ship_types)))



header <- dashboardHeader(
  color = "blue",
  inverted = TRUE
)


sidebar <- dashboardSidebar(
  color = "teal",
  sidebarMenu(
    menuItem(text = "Home", 
             tabName = "home",
             icon = icon("home", lib = "glyphicon")
    )
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "home",
      fluidRow(
        column(
          width = 12,
          # dropdown_mod_ui("input_vars"),
          div(style = "display: inline-block;vertical-align:top; width: 250px;",
              selectInput(
                inputId = "ship_type",
                label = "Select ship type:",
                choices = ship_types,
                width = "250px")
          ),
          div(style = "display: inline-block;vertical-align:top; width: 50px;", HTML("<br>")),
          div(style = "display: inline-block;vertical-align:top; width: 250px;",
              selectInput(
                inputId = "ship_name",
                label = "Select ship name:",
                choices = "",
                width = "250px")
          ),
          div(style = "display: inline-block;vertical-align:top; width: 80px;", HTML("<br>")),
          div(style = "display: inline-block;vertical-align:top; width: 250px;",
              valueBoxOutput(
                outputId = "meters_box")
          )
        )
      ),
      leafletOutput(
        outputId = "leaflet_map",
        width = "100%", 
        height = 550
      )
    )
  ),
  useShinyjs()
)


ui <- dashboardPage(
  header = header, 
  sidebar = sidebar, 
  body = body,
  title = "Exploration of the Marine data",
  theme = "paper"
)


server <- function(input, output, session) {
  
  # ns <- session$ns
  
  # inputvars <- callModule(varselect_mod_server, id = "input_vars")
  # inputvars <- callModule(varselect_mod_server, id = "input_vars", input$input_vars[["ship_type"]])
  
  observe({
    shinyjs::disable("ship_name")
    shinyjs::hide("leaflet_map")
  })

  observeEvent(input[["ship_type"]], {
  # observeEvent(inputvars$ship_type(), {
    
    # ns <- session$ns
    
    updateSelectInput(
      session,
      inputId = "ship_name",
      # inputId = ns("ship_name"),
      label = "Select ship name",
      choices = list_ship_types[[input$ship_type]])
      # choices = list_ship_types[[inputvars$ship_type()]])
    
    if (input[["ship_type"]] != "") {
    # if (inputvars$ship_type() != "") {
      shinyjs::enable("ship_name")
      shinyjs::show("leaflet_map")
    }
  })
    
  df_points <- reactive({
    df <- df_data %>%
      filter(ship_type == input[["ship_type"]],
             # ship_type == inputvars$ship_type(),
             SHIPNAME == input[["ship_name"]])
             # SHIPNAME == inputvars$ship_name())
  })
  
  output$meters_box <- renderValueBox({
    valueBox(
      subtitle = "Maximal distance [m]",
      value = round(unique(df_points()$max_dist)),
      icon = icon("credit-card")
    )
  })
  
  output[["leaflet_map"]] <- renderLeaflet({
    req(df_points())

    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)}") %>%
      addTiles() %>%
      addMarkers(lng = df_points()$LON, lat = df_points()$LAT) %>%
      addPolylines(lng = df_points()$LON, lat = df_points()$LAT, color = "red")
  })
  
}

shinyApp(ui = ui, server = server)

