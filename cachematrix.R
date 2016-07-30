## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix: This function creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverseOfMatrix) xinv <<- inverseOfMatrix
  getinverse <- function() xinv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}


## Write a short comment describing this function
# cacheSolve: This function computes the inverse of the matrix returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves 
# the inverse from the cache, else it computes and caches the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xinv <- x$getinverse()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data)
  x$setinverse(xinv)
  xinv
  
}

## Example
mySpecialVector <- makeCacheMatrix(matrix(2:5, nrow = 2, ncol = 2))
cacheSolve(mySpecialVector)
