
## The function makeCacheMatrix makes a data object that contains a matrix (mutable through set/get) and its inverse

makeCacheMatrix <- function(x = matrix()) {
 
  #initialize inverse
  inv <- NULL
  
  #function to set new matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #function to get stored matrix
  get <- function() x
  
  #function to set inverse matrix
  set_inverse <- function(inverse_matrix) inv <<- inverse_matrix
  
  #funtction to get inverse matrix
  get_inverse <- function() inv
  
  list(set=set, get=get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## The function cacheSolve has an envirmonment linked with the makeCachemMatrix; if the inverse of the matrix is already known, 
## it returns the known inverse, otherwise it calculates it from scratch.

cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()
  if(!is.null(inv)){
    message("Getting cached data...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$set_inverse(inv)
  inv
}
