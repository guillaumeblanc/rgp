source('R/scrape.R')
source('R/cache.R')
source('R/keras.R')

library('rjson')

main <- function() {
  # Loads configuration
  config <- fromJSON(file = 'config.json')
  
  # Updates source data
  # scrape.classifications(config$scrape)
  
  train(config$keras)
}

main()
