## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix makes a data object that contains a matrix (mutable through set/get) and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse_matrix) inv <<- inverse_matrix
  get_inverse <- function() inv
  
  list(set=set, get=get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## The function cacheSolve has an envirmonment linked with the makeCachemMatrix; if the inverse of the matrix is already known, 
## it returns the known inverse, otherwise it calculates it from scratch.

cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()
  if(!is.null(inv)){
    print("Getting cached data...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$set_inverse(inv)
  inv
}
