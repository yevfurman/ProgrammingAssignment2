## Functions to create a Cached matrix and to calculate it's inverse

## makeCacheMatrix gets a matrix as input. set sets the value of the matrix,
## get returns the value of the matrix, setinv sets the value of the inverse matrix
## and getinv returns the value of the cached matrix

makeCacheMatrix<-function(x=matrix()){
      m<-NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv= setinv,
           getinv = getinv)
}


## cacheSolve gets a cached matrix as input. If it's inverse has already been calculated (m isn't NULL)
## then the cached value is returned. Otherwise it calculates it's inverse.

cacheSolve <- function(x, ...) {
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}
