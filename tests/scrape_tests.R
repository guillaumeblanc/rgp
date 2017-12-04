source('R/scrape.R')

library('RUnit')

test.scrape_classification <- function()
{
  config <- list()
  config$classification_data <- 'tests/data/classification'
  
  dir.create(file.path(config$classification_data), recursive = TRUE, showWarnings = FALSE)
  
  # Loads a know classification
  checkTrue(scrape.classification(c('2017', 'VAL', 'FP4'), config))
  
  # Checks results
  results <- data.frame(read.csv(scrape.output_path('2017', 'VAL', 'FP4', 'Classification', config)))
  # print(results)
    
  checkEquals(results$Pos.[[1]], 1)
  checkEquals(results$Gap[[1]], 0)
  checkEquals(results$Gap[[25]], 2.279)
  checkEquals(results$Time[[8]], 91.651)
  checkEquals(which(results['Rider'] == 'Valentino ROSSI'), 8)
}

test.scrape_rac_classification <- function()
{
  config <- list()
  config$classification_data <- 'tests/data/classification'

  dir.create(file.path(config$classification_data), recursive = TRUE, showWarnings = FALSE)

  # Loads a know classification
  checkTrue(scrape.classification(c('2017', 'VAL', 'RAC'), config))

  # Checks results
  results <- data.frame(read.csv(scrape.output_path('2017', 'VAL', 'RAC', 'Classification', config)))

  checkEquals(which(results$Num. == 46), 5)
  checkTrue(results$Pos.[results$Num. == 99] == 'DNF')
}

test.scrape_condition <- function()
{
  config <- list()
  config$classification_data <- 'tests/data/classification'
  
  dir.create(file.path(config$classification_data), recursive = TRUE, showWarnings = FALSE)
  
  # Loads a know classification
  checkTrue(scrape.classification(c('2017', 'VAL', 'RAC'), config))
  
  # Checks results
  results <- data.frame(read.csv(scrape.output_path('2017', 'VAL', 'RAC', 'Conditions', config)))

  checkTrue(results$Value[which(results$Title == 'Track Condition')] == 'Dry')
  checkTrue(results$Value[which(results$Title == 'Ground')] == 26)
  checkTrue(results$Value[which(results$Title == 'Air')] == 25)
  checkTrue(results$Value[which(results$Title == 'Humidity')] == 22)
}

test.scrape_qp_classification <- function()
{
  config <- list()
  config$classification_data <- 'tests/data/classification'
  
  dir.create(file.path(config$classification_data), recursive = TRUE, showWarnings = FALSE)
  
  # QP is merged from Q1 and Q2
  checkTrue(scrape.classification(c('2017', 'VAL', 'Q1'), config))
  checkTrue(scrape.classification(c('2017', 'VAL', 'Q2'), config))
  
  # Checks results
  results <- data.frame(read.csv(scrape.output_path('2017', 'VAL', 'QP', 'Classification', config)))
  checkEquals(which(results$Num. == 93), 1)
  checkEquals(which(results$Num. == 43), 12)
  checkEquals(which(results$Num. == 25), 13)
}

test.scrape_classification_invalid <- function()
{
  # Loads a invalid classification (Quatar 2017 had no FP4)
  checkException(scrape.classification(c('2017', 'QAT', 'FP4'), config))
}
