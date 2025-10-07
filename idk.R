library(plumber)

#* @apiTitle Simple Test API

#* Health check endpoint
#* @get /ping
function() {
  list(message = "pong")
}
