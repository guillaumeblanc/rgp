source('R/scrape.R')
source('R/cache.R')

library('rjson')

main <- function() {
  # Loads configuration
  config <- fromJSON(file = 'config.json')
  
  # Updates source data
  scrape.classifications(config)
}

main()
