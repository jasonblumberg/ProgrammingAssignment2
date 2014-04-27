## Put comments here that give an overall description of what your
## functions do

# Function creates a special vector of other functions for use in setting a matrix as an input,
# getting or recalling the current matrix, setting the result of an inverse operation performed on
# the current matrix using 'cacheSolve' and retrieving the value previously set

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                                          ## Create a null vector to hold matrix
  set <- function(y) {                               ## define the 'set' function which stores a given matrix,
    x <<- y                                          ## (any matrix can be input using this function)
    i <<- NULL
  }
  get <- function() x                                ## Define 'get' function which returns currently stored matrix
  setinverse <- function(inverse) i <<- inverse      ## Define 'setinverse' function which holds cached inverse 
  getinverse <- function() i                         ## Define 'getinverse' function which returns currently  cached inverse
  list(set = set, get = get,                         ## Output of this function which is four further functions
       setinverse = setinverse,
       getinverse = getinverse)
}

# Function checks to see if the inverse of a given matrix has been computed. If it has the function returns the,
# cached value, if not the function computes the inverse and stores it to the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()                                ## Function retrieves the value stored in the assigned cache
  if(!is.null(i)) {                                  ## If the value exists the function returns it
    message("getting cached data")
    return(i)
  }
  data <- x$get()                                    ## If the value does not exist the function retrieves the given matrix
  i <- solve(data, ...)                              ## Function computes matrix inverse
  x$setinverse(i)                                    ## Function stores inverse result in cache
  i                                                  ## Returns the inverse result to the console
}