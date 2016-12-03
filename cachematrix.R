## Create a matrix and cache the inverse

## Create the matrix that can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inv) m <<- inv
    
    getinv <- function() m
    
    list(set = set, get = get,setinv = setinv,getinv = getinv)
    
}

## solve for the matrix inverse and cache the result. Return the value from cache when it is valid
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m<-solve(data)
    x$setinv(m)
    m
}
