library(shiny)
library(leaflet)
library(httr)
library(jsonlite)

API_URL <- "http://127.0.0.1:8000/bus-location"

start_location <- list(lat = 28.6139, lon = 77.2090)  # Connaught Place, Delhi
end_location <- list(lat = 28.6400, lon = 77.2500)    # Destination in Delhi

# UI
ui <- fluidPage(
  titlePanel("Real-Time Bus Tracking"),
  leafletOutput("bus_map", height = "600px"),
  tags$script(HTML("
    setInterval(function() {
      Shiny.onInputChange('update_bus', Math.random());
    }, 1000);
  ")) # Auto-refresh every 3 seconds
)

server <- function(input, output, session) {
  
  bus_location <- reactiveVal(NULL)
  
  updateBusLocation <- function() {
    res <- try(GET(API_URL), silent = TRUE)
    if (!inherits(res, "try-error") && http_status(res)$category == "Success") {
      data <- fromJSON(content(res, as = "text"))
      bus_location(data)
    }
  }
  
  updateBusLocation()
  
  observeEvent(input$update_bus, {
    updateBusLocation()
  })
  
  output$bus_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = start_location$lon, lat = start_location$lat, zoom = 14) %>%
      
      addMarkers(lng = start_location$lon, lat = start_location$lat, 
                 popup = "Start Location", icon = makeAwesomeIcon(icon = "flag", markerColor = "green")) %>%
      addMarkers(lng = end_location$lon, lat = end_location$lat, 
                 popup = "End Location", icon = makeAwesomeIcon(icon = "flag", markerColor = "red"))
  })
  
  observe({
    loc <- bus_location()
    if (!is.null(loc)) {
      leafletProxy("bus_map") %>%
        clearGroup("bus") %>%
        addMarkers(lng = loc$lon, lat = loc$lat, popup = "Bus is here!", 
                   icon = makeIcon(
                     iconUrl = "https://img.icons8.com/?size=100&id=16195&format=png&color=000000",
                     iconWidth = 32, iconHeight = 32
                   ),
                   group = "bus")
    }
  })
}

# Run the app
shinyApp(ui, server)

