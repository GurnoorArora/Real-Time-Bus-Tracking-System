
library(shiny)
library(leaflet)
library(httr)
library(jsonlite)

# API URL (update this if running on a different machine)
API_URL <- "http://127.0.0.1:8000/bus-location"

# UI
ui <- fluidPage(
  titlePanel("Real-Time Bus Tracking"),
  leafletOutput("bus_map", height = "600px"),
  tags$script(HTML("
    setInterval(function() {
      Shiny.onInputChange('update_bus', Math.random());
    }, 3000);
  ")) # Auto-refresh every 3 seconds
)

# Server
server <- function(input, output, session) {
  
  # Reactive value to store bus location
  bus_location <- reactiveVal(NULL)
  
  # Function to fetch bus location from API
  updateBusLocation <- function() {
    res <- try(GET(API_URL), silent = TRUE)
    if (!inherits(res, "try-error") && http_status(res)$category == "Success") {
      data <- fromJSON(content(res, as = "text"))
      bus_location(data)
    }
  }
  
  # Fetch initial location
  updateBusLocation()
  
  # Update location when the JS trigger fires
  observeEvent(input$update_bus, {
    updateBusLocation()
  })
  
  # Render map
  output$bus_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 77.2090, lat = 28.6139, zoom = 14)
  })
  
  # Update marker when bus location changes
  observe({
    loc <- bus_location()
    if (!is.null(loc)) {
      leafletProxy("bus_map") %>%
        clearMarkers() %>%
        addMarkers(lng = loc$lon, lat = loc$lat, popup = "Bus is here!")
    }
  })
}

# Run the app
shinyApp(ui, server)
