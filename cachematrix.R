## Put comments here that give an overall description of what your
## functions do.

#These functions will create a matrix, compute its inverse and cache it.
#They will pull this cached information when needed if the same matrix 
#is called.

## Write a short comment describing this function. This function makes the
#special matrix object.

makeCacheMatrix <- function(MATX = matrix()) {
  # this is storing the inverse 
  inverse <- NULL
  
  #this is setting the matrix and the inverse
  set <- function(y) {
    MATX <<- y
    inverse <<- NULL
  }
  #this is getting the matrix
  get <- function() MATX
  
  #this is setting and getting the inverse 
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  
  # this is returning a list of the 4 functions
  #to set or get the matrix or the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function.
#This function will generate the inverse of the matrix. If the matrix
# is the same and if the inverse has been calculated, then it will 
#retrieve it from the cache and indicate it by displaying "retrieving cached data".

cacheSolve <- function(spec_matrix, ...) {
  inverse <- spec_matrix$getinverse()
  if(!is.null(inverse)) {
    message("retrieving cached data")
    return(inverse)
  }
  data <- spec_matrix$get()
  inverse <- solve(data, ...)
  spec_matrix$setinverse(inverse)
  inverse
}


