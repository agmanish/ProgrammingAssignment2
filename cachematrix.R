## Put comments here that give an overall description of what your
## functions do

## Functions are defined below that allow us to cache(store in memory) the 
## inverse of a matrix for future computation, to speed up computing by not 
##calculating the inverse each time.

##the makeCacheMatrix function defines an object that can cache the matrix inverse
##and returns it when called. 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(new) {
    x <<- new 
    inv <<- NULL
  }
  get <- function() x 
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
##the below function checks if a cache of the inverse exists else it 
## calculates it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
