library('rvest')
library('htmltidy')
library('parallel')
library('plyr')
library('dplyr')
library('stringr')
library('doParallel')

scrape.url <- function(year, circuit, session, suffix) {
  paste('http://www.motogp.com/en/Results+Statistics', year, circuit, 'MotoGP', session, suffix, sep='/')
}
scrape.html_path <- function(year, circuit, session, suffix, config) {
  file.path(config$data_html, paste0(paste(year, circuit, session, suffix, sep = '_'),'.html'))
}
scrape.raw_path <- function(year, circuit, session, suffix, config) {
  file.path(config$data_raw, paste0(paste(year, circuit, session, suffix, sep = '_'), '.csv'))
}
library('doParallel')

scrape.time <- function(str) {
  tm <- str_match(str, '(?:([0-9])\')?([0-9]{1,2}.[0-9]{3})')
  tm[is.na(tm[, 2]), 2] <- 0
  as.numeric(tm[,2]) * 60 + as.numeric(tm [,3])
}

scrape.results <- function(year, circuit, session, nodes) {
  
  # Extract classification table
  table <- html_table(html_node(nodes, xpath='//*[@id="main_result"]/table[1]'))
  df <- data.frame(table)
  
  # Renames columns
  names(df) <- gsub("\\.", "", names(df))
  names(df) <- tolower(names(df))
  
  # Removes irrelevant lines, aka those with no rider number
  df$num <- suppressWarnings(as.integer(df$num))
  df <- df[!is.na(df$num),]
  
  # Convert positions to integers, keeps NA for DNF
  df$pos <- suppressWarnings(as.integer(df$pos))
  
  if(session == 'RAC') {
    # Fix up empty points
    df$points[df$points == ''] <- '0'
    df$points <- suppressWarnings(as.integer(df$points))
  } else {
    # Renames gap column
    df <- rename(df, gap = gap1stprev)
    
    # Fix up empty entrie (1st rider)
    df$gap[1] <- '0.000 / 0.000'
    
    # Only keeps gap-to-first, strip gap-to-prev
    df$gap <- scrape.time(df$gap)
    
    # Convert minutes to seconds
    df$time <- scrape.time(df$time)
  }
  
  df # returns
}

scrape.conditions <- function(year, circuit, session, nodes) {
  
  # Track condition
  text <- html_text(html_node(nodes, xpath='//*[@id="main_result"]/span[3]'))
  tc <- str_match(text, 'Track Condition: (.*)')
  
  # Air
  text <- html_text(html_node(nodes,
                              xpath='//*[@id="main_result"]/span[5]'))
  ta <- str_match(text, 'Air: (.*)º')
  
  # Humidity
  text <- html_text(html_node(nodes,
                              xpath='//*[@id="main_result"]/span[7]'))
  th <- str_match(text, 'Humidity: (.*)%')
  
  # Ground
  text <- html_text(html_node(nodes,
                              xpath='//*[@id="main_result"]/span[9]'))
  tg <- str_match(text, 'Ground: (.*)º')
  
  if(length(tc) != 2 | length(ta) != 2 | length(th) != 2 | length(tg) != 2) {
    stop('missing')
  }
  
  # Builds and clean data.frame
  l <- list(condition = tc[2],
            ground = as.integer(tg[2]),
            air = as.integer(ta[2]),
            humidity =  as.integer(th[2]))

  l # returns
}

scrape.merge_qp <- function(csvs) {
  # Clean up Q1
  q1 <- csvs[[1]]
  q1 <- q1[-c(1, 2),]  # Removes first 2 positions
  
  # Compose the 2 QP
  df <- rbind(csvs[[2]], q1)
  
  # Fix all positions
  df$pos <- 1:length(df$pos)
  
  # Rebuild gaps
  df$gap <- sapply(df$gap, function(x) {x - df$gap[[1]]})
  
  df # returns
}

fetch <- function(comb, config) {
  # Prepares output folder
  dir.create(file.path(config$data_html), recursive = TRUE, showWarnings = FALSE)
  
  to_download <- comb[!comb$html_exist, ]
  print(sprintf('Downloading %d/%d entries...', nrow(to_download), nrow(comb)))
  
  cluster <- makeCluster(config$maxconnects)
  clusterEvalQ(cluster, library('htmltidy'))
  registerDoParallel(cluster)
  
  download <- function(url_path, html_path) {
    html <- tidy_html(url(url_path), list(TidyDocType="html5", TidyWrapLen=0))
    # Considers data invalid if pos cannot be found
    valid <- grepl('>Pos.<', html, fixed = TRUE)
    if(valid) {
      writeLines(html, html_path)
    }
    return(valid)
  }
  perf <- system.time(to_download$html_exist <- unlist(alply(to_download, 1, function(r) download(r$url, r$html), .parallel = TRUE)))
  stopCluster(cluster)
  
  comb[!comb$html_exist, 'html_exist'] <- to_download$html_exist
  print(sprintf('Dowload completed, %d new entries found in %0.1fs.', sum(to_download$html_exist), perf['elapsed']))

  comb # return  
}

scrape <- function (config) {
  
  # Builds combination table, urls and already exiting data
  comb <- expand.grid(year = config$years, circuit = config$circuits, session = config$sessions)
  comb <- arrange(comb, year, circuit, session)
  comb$url <- apply(comb, 1, function(x) scrape.url(x[1], x[2], x[3], 'Classification'))
  comb$html <- apply(comb, 1, function(x) scrape.html_path(x[1], x[2], x[3], 'classification', config))
  comb$html_exist <- file.exists(unlist(comb$html))
  
  # Downloads
  if(config$download) {
    comb <- fetch(comb, config)
  } else {
    print(sprintf('Fecth disabled, %d / %d entries available.', sum(comb$html_exist), nrow(comb)))
  }
  
  # Scrape
  print('Scraping...')
  dir.create(file.path(config$data_raw), recursive = TRUE, showWarnings = FALSE)

  perf <- system.time(a_ply(comb[comb$html_exist,], 1, function(key, config) {
    nodes <- read_html(key$html)

    df <- scrape.results(key$year, key$circuit, key$session, nodes)
    filename <- scrape.raw_path(key$year, key$circuit, key$session, 'classification', config)
    write.csv(df, file = filename, row.names = FALSE)

    df <- scrape.conditions(key$year, key$circuit, key$session, nodes)
    filename <- scrape.raw_path(key$year, key$circuit, key$session, 'condition', config)
    write.csv(df, file = filename, row.names = FALSE)
  }, config, .progress = 'text'))
  print(sprintf('Scraped %d classifications/conditions in %0.1fs.', nrow(comb), perf['elapsed']))
  
  print('Merging QP...')
  perf <- system.time(a_ply(comb[comb$session == 'QP' & !comb$html_exist,], 1, function(key, config) {
    # Merges Q1 and Q2
    filenames <- scrape.raw_path(key$year, key$circuit, c('Q1','Q2'), 'classification', config)
    if(all(file.exists(filenames))) {
      csvs <- lapply(filenames, read.csv)
      df <- scrape.merge_qp(csvs)
      filename <- scrape.raw_path(key$year, key$circuit, 'QP', 'classification', config)
      write.csv(df, file = filename, row.names = FALSE)
    }
    # Copies Q2 conditions to QP
    file.copy(scrape.raw_path(key$year, key$circuit, 'Q2', 'condition', config),
              scrape.raw_path(key$year, key$circuit, 'QP', 'condition', config),
              overwrite = TRUE)
    
  }, config, .progress = 'text'))
}
