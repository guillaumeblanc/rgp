source('R/scrape.R')

library('rjson')

main <- function() {
  # Loads configuration
  config <- fromJSON(file = 'config.json')
  
  # Updates html and raw data
  scrape(config)
}

main()