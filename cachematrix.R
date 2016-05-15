##The following functions receives a matrix as an input
##and creates a list of functions get, set, getinverse and setinverse
## that can be used to calculate the inverse of a matrix
## using accessors and in will be used in the other cacheSolve
## function to actually cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##define the set accessor
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##define the get accessor
  get <- function() x
  ##define the setinverse set accesor
  setinverse <- function(inverse) m <<- inverse
  ##define the getinverse get accessor
  getinverse <- function() m
  ##finaly a list with the above 4 functions will be returned
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## this functions will actually look for the cache entry for the 
## inverse matrix and if defined, it will return such entry
## and will print out 'getting cache data' if the cache entry is found
## otherwise it will cache the entry and save it for later use
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    ##If the cache entry is already there, just returns it
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    ##if not, then call solve function and set the entry
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
