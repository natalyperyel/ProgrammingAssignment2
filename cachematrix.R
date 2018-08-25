## The first function, makeCacheMatrix creates a list that stores: set the value of the matrix,
## get the value of the matrix, set the value of the inverse, get the value of the inverse.


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The second function cacheSolve returnes by makeCacheMatrix above. 
## If the inverse has already been calculated, cacheSolve should get the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## b <- matrix(1:16, 2, 2)
        ## c <- makeCacheMatrix(B)
        ## cacheSolve(c)
    
        ##      [,1] [,2]
        ##      [1,]   -2  1.5
        ##      [2,]    1 -0.5
}
