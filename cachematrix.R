## These two function calculate and cache inverse of given matrix. If inverse 
## matrix was already computed, functions get it from cache

## The first function creates a special "matrix" which is a list of 4 functions 
## to 1) Set a matrix; 2) get a matrix; 3) set the inverse matrix; 4) get the 
## inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
                        x <<- y
                        m <<- NULL
                        }
    get <- function() x
    set_inv_matrix <- function(inv_matrix) m <<- inv_matrix
    get_inv_matrix <- function() m
    list(set = set, get = get,
         set_inv_matrix = set_inv_matrix,
         get_inv_matrix = get_inv_matrix)
}


## The second function checks if the inverse matrix stored in cache and if yes, 
## loads it from cache. Otherwise it computes inverse matrix using solve() 
## function and stores it in cache.

cacheSolve <- function(x, ...) {
    m <- x$get_inv_matrix()
    if (identical(m, x$get()) && (!is.null(m))) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set_inv_matrix(m)
    m
}
