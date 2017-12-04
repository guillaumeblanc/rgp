source('R/cache.R')

library('RUnit')

test.cache <- function() {
  val <- 0
  cache <- make_cache(function(key) { val <<- val + 1 })
  
  checkEquals(cache('a'), 1)
  checkEquals(cache('a'), 1)
  checkEquals(cache('b'), 2)
  checkEquals(cache('a'), 1)
  checkEquals(cache('c'), 3)
  
  checkEquals(lapply(c('b', 'a', 'b', 'd', 'c'), cache),
              list(2, 1, 2, 4, 3))
  
  checkEquals(cache('d'), 4)
  checkEquals(cache('c'), 3)
}

test.cache_exception <- function() {
  val <- 0
  cache <- make_cache(function(key) {
    if(key == 'e') {
      stop('An error...')
    } else {
      val <<- val + 1
    }
  })
  
  checkEquals(cache('a'), 1)
  checkEquals(cache('a'), 1)
  checkEquals(cache('b'), 2)
  checkException(cache('e'))
  checkException(cache('e'))
  checkEquals(cache('a'), 1)
  checkEquals(cache('b'), 2)
  checkEquals(cache('c'), 3)
  
  checkEquals(lapply(c('b', 'a', 'b', 'e', 'd', 'c'),
                     function(k) tryCatch(cache(k), error = function(e){return(0)})),
              list(2, 1, 2, 0, 4, 3))
  
  checkEquals(cache('d'), 4)
  checkEquals(cache('c'), 3)
}

test.cache_vkey <- function() {
  val <- 0
  cache <- make_cache(function(key) { val <<- val + 1 })
  
  checkEquals(cache(c('a')), 1)
  checkEquals(cache(c('a', 'a')), 2)
  checkEquals(cache(c('a')), 1)
  checkEquals(cache(1:10), 3)
}

test.cache_vaargs <- function() {
  val <- 0
  cache <- make_cache(function(key, other) {
    ifelse(key == 'e' & other == 'e', 1, 2)
  })
  
  checkEquals(cache('a', 'a'), 2)
  checkEquals(cache('b', 'e'), 2)
  checkEquals(cache('e', 'e'), 1)
}
