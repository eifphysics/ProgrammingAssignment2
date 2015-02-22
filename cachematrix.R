## get inverse of a matrix and cache its value 

## set the value of a matrix, get its value,
##set its inverse and get its inverse, and create
##a special "list matrix" containing these data 

makeCacheMatrix <- function(x = matrix()) {
      cache <- NULL
      setMatrix <- function(newValue){            
            x <<- newValue
            cache <<- NULL
      }
      getMatrix <- function() x
      setInverse <- function(inverse) cache <<- inverse
      getInverse <- function() cache
      list(setMatrix = setMatrix, getMatrix = getMatrix,
           setInverse = setInverse,
           getInverse = getInverse)
}


## compute value of a matrix, retrive a  cache value
##if it is already computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      cache <- x$getInverse()
      if(!is.null(cache)){
            message("getting cache data")
            return(cache)
      }
      data <- x$getMatrix()
      cache <- solve(data, ...)
      x$setInverse(cache)
      cache
}
