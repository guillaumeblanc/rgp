# Implements MotoGP classification results scraping.

source('R/cache.R')

library('rvest')
library('htmltidy')
library('stringr')

scrape.build_url <- function(year, circuit, session, suffix) {
  paste('http://www.motogp.com/en/Results+Statistics', year, circuit, 'MotoGP', session, suffix, sep='/')
}

scrape.output_path <- function(year, circuit, session, suffix, config) {
  file.path(config$classification_data, paste(paste(year, circuit, session, suffix, sep = '_'),
                                              '.csv', sep = ''))
}

scrape.classification.results <- function(html, key, config) {
  # Extract classification table
  table <- html_table(html_node(read_html(html),
                                xpath='//*[@id="main_result"]/table[1]'))
  df <- data.frame(table)
  
  # Renames some column
  colnames(df)[colnames(df) == 'Pos.'] <- 'Pos'
  colnames(df)[colnames(df) == 'Num.'] <- 'Num'
  colnames(df)[colnames(df) == 'Km.h'] <- 'Kmh'
  
  # Removes irrelevant lines, aka those with no rider number
  df$Num <- suppressWarnings(as.integer(df$Num))
  df <- df[!is.na(df$Num),]
  
  # Convert positions to integers, keeps NA for DNF
  df$Pos <- suppressWarnings(as.integer(df$Pos))
  
  if(key[[3]] == 'RAC') {
    # Fix up empty points
    df$Points[df$Points == ''] <- '0'
    df$Points <- suppressWarnings(as.integer(df$Points))
  } else {
    # Renames Gap column
    colnames(df)[colnames(df) == 'Gap.1st.Prev.'] <- 'Gap'
    
    # Fix up empty entrie (1st rider)
    df$Gap[df$Gap == ''] <- '0.0 / 0.0'
    
    # Only keeps Gap-to-first, strip gap-to-prev
    df$Gap <- as.numeric(str_extract(df$Gap, '([0-9]+.[0-9]+)'))
    
    # Convert minutes to seconds
    tm <- str_match(df$Time, '([0-9]+)\'([0-9]{2}.[0-9]{3})')
    df$Time <- as.numeric(tm[,2]) * 60 + as.numeric(tm [,3])
  }
  
  # Creates output filename and writes it.
  filename <- scrape.output_path(key[1], key[2], key[3], 'classification', config)
  print(sprintf('Writing scrapped classification data to "%s"', filename))
  write.csv(df, file = filename, row.names = FALSE)
}

scrape.classification.conditions <- function(html, key, config) {
  # Track condition
  text <- html_text(html_node(read_html(html),
                              xpath='//*[@id="main_result"]/span[3]'))
  tc <- str_match(text, '(.*): (.*)')

  # Air
  text <- html_text(html_node(read_html(html),
                              xpath='//*[@id="main_result"]/span[5]'))
  ta <- str_match(text, '(.*): (.*)º')
  
  # Humidity
  text <- html_text(html_node(read_html(html),
                              xpath='//*[@id="main_result"]/span[7]'))
  th <- str_match(text, '(.*): (.*)%')
  
  # Ground
  text <- html_text(html_node(read_html(html),
                              xpath='//*[@id="main_result"]/span[9]'))
  tg <- str_match(text, '(.*): (.*)º')
  
  # Builds and clean data.frame
  df <- rbind(tc[2:3], tg[2:3], ta[2:3], th[2:3])
  colnames(df) <- c('Title', 'Value')

  # Creates output filename and writes it.
  filename <- scrape.output_path(key[1], key[2], key[3], 'conditions', config)
  print(sprintf('Writing scrapped conditions data to "%s"', filename))
  write.csv(df, file = filename, row.names = FALSE)
}

scrape.classification.merge_qp <- function(key, config) {
  if(any(key[[3]] == c('Q1','Q2'))) {
    filenames <- scrape.output_path(key[1], key[2], c('Q1','Q2'), 'classification', config)
    if(!all(file.exists(filenames))) {
      return()
    }
    csvs <- lapply(filenames, read.csv)
    
    # Clean up Q1
    q1 <- csvs[[1]]
    q1 <- q1[-c(1, 2),]  # Removes first 2 positions
    
    # Compose the 2 QP
    df <- rbind(csvs[[2]], q1)
    
    # Fix all positions
    df$Pos <- 1:length(df$Pos)
    
    # Rebuild Gaps
    df$Gap <- sapply(df$Gap, function(x) {x - df$Gap[[1]]})
    
    # Creates output filename and writes it.
    filename <- scrape.output_path(key[1], key[2], 'QP', 'classification', config)
    print(sprintf('Writing scrapped conditions data to "%s"', filename))
    write.csv(df, file = filename, row.names = FALSE)
  }
}

scrape.classification <- function(key, config) {
  
  # Generate url from key
  path <- scrape.build_url(key[1], key[2], key[3], 'Classification')
  print(paste('Scraping url "', path, '"'))
  
  # Download and tidy html
  tidied_html <- tidy_html(url(path), list(TidyDocType="html5", TidyWrapLen=0))
  
  # Scrape
  scrape.classification.results(tidied_html, key, config)
  scrape.classification.conditions(tidied_html, key, config)
  scrape.classification.merge_qp(key, config)
  
  TRUE  # return
}

scrape.classifications <- function(config) {
  # Prepares output folder
  dir.create(file.path(config$classification_data), recursive = TRUE, showWarnings = FALSE)
  
  # Creates or loads cache if it exists
  scrape_cache <- make_cache(scrape.classification)
  cache_filename = file.path(config$classification_data, 'classification_cache.RData')
  if(config$load_cache & file.exists(cache_filename)) {
    print('Loading existing scraping cache')
    scrape_cache <- readRDS(cache_filename)  # Loads directly in scrape_cache
  }

  # Combine all entries
  urls <- expand.grid(year = config$years, circuit = config$circuits, session = config$sessions)

  # Scraping
  print('Scraping classifications urls')
  apply(urls, 1, function(key) tryCatch(
    scrape_cache(key, config),
    error = function(e) { if(grepl('xml_missing',e)) { print('No data available.') } else { stop(e) }}))
  
  # Dumps cache in config$classification_data
  saveRDS(scrape_cache, file = cache_filename)
}

