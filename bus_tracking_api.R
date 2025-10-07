library(plumber)
library(jsonlite)
library(httr)
library(geosphere)  

api_key <- "AIzaSyChAZUL3ihIu2S5cfNZVs3Trh7KMNgUroc"
origin <- "28.6139,77.2090"        # Connaught Place, Delhi
destination <- "28.6400,77.2500"   # Some location in Delhi


url <- paste0(
  "https://maps.googleapis.com/maps/api/directions/json?",
  "origin=", origin, "&destination=", destination,
  "&key=", api_key,
  "&mode=driving"
  
)
response <- GET(url)
data <- content(response, "parsed")

route_steps <- data$routes[[1]]$legs[[1]]$steps

interpolate_points <- function(start, end, n = 10) {
  lats <- seq(start[1], end[1], length.out = n)
  lons <- seq(start[2], end[2], length.out = n)
  return(mapply(function(lat, lon) list(lat = lat, lon = lon), lats, lons, SIMPLIFY = FALSE))
}

route_coords <- list()
for (step in route_steps) {
  start <- c(step$start_location$lat, step$start_location$lng)
  end <- c(step$end_location$lat, step$end_location$lng)
  route_coords <- append(route_coords, interpolate_points(start, end, n = 15))  # More points per step
}

bus_index <- 1

#* @apiTitle bus_tracking_api

#* Get Live Bus Location
#* @get /bus-location
function() {
  if (bus_index > length(route_coords)) {
    bus_index <<- 1  # Reset to the start of the route
  }
  
  location <- route_coords[[bus_index]]
  bus_index <<- bus_index + 1  
  
  return((list(lat = location$lat, lon = location$lon)))
}
