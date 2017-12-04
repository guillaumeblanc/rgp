# Implements a cache system based on a closure.
# Provided function fct is only called once, when the key isn't know, thus the result isn't cached. 

make_cache <- function(fct) {
  
  # Initialize the cache
  cache <- new.env(hash = TRUE, parent = emptyenv())
  
  cache_set <- function(key, value) {
    assign(key, value, envir = cache)
  }
  
  cache_get <- function(key) {
    get(key, envir = cache, inherits = FALSE)
  }
  
  cache_has_key <- function(key) {
    exists(key, envir = cache, inherits = FALSE)
  }
  
  # Closure function
  function(key, ...) {  # return
    hash <- paste(key, collapse = '')
    if(!cache_has_key(hash)) {
      cache_set(hash, fct(key, ...))
    }
    cache_get(hash)  # return
  }
}
