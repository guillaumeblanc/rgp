library(keras)

load_raw <- function(config, suffix) {
  pattern <- paste0('(', paste0(config$years, collapse = '|'), ').*', suffix, '\\.csv')
  files <- list.files(path = file.path(config$classification_data), pattern = pattern, full.names = TRUE)
  cat(length(files), 'csv files found for pattern:', pattern, '\n')
  csvs <- lapply(files, read.csv)
  csvs  # return
}

train <- function(config) {
  # Get raw data
  train_pattern <- paste0('(', paste0(config$train_sessions, collapse = '|'), ')_classification')
  train <- load_raw(config, train_pattern)
  target_pattern <- paste0('(', paste0(config$target_sessions, collapse = '|'), ')_classification')
  target <- load_raw(config, target_pattern)
  
  # Filter a rider
  rider <- function(x) x[x$Num. == 46, 'Pos.']
  train <- lapply(train, rider)
  target <- lapply(target, rider)
  
  # structure data
  train_arr <- array_reshape(train, c(length(train), 1))
  target_cat <- to_categorical(target, 25)
  
  # print(train)
  # print(target_cat)
  
  model <- keras_model_sequential() 
  model %>% 
    layer_dense(units = 32, activation = 'relu', input_shape = c(1)) %>% 
    # layer_dropout(rate = 0.4) %>%
    # layer_dense(units = 128, activation = 'relu') %>%
    # layer_dropout(rate = 0.3) %>%
    layer_dense(units = ncol(target_cat), activation = 'softmax')
  
  summary(model)
  
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy')
  )
  
  # tensorboard("logs/run_a")
  
  history <- model %>% fit(
    train_arr, target_cat, 
    epochs = 100, batch_size = length(target),
    # callbacks = callback_tensorboard("logs/run_a"),
    validation_split = 0.2
  )
  
  plot(history)
  
  # model %>% evaluate(x_test, y_test)
  
  # model %>% predict_classes(x_test)
}