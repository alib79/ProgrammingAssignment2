## The code caches inverse of a matrix so that whenever needed again the value is 
## is read from Cache memory instead of recalculation

## The first function generates a special matrix which is a list of four functions set, get, setInv, and getInv

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(Inv) m <<- Inv
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## calcules inverse of the special matrix while it first checks if it is cached
## in this case it is read from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,diag(nrow(data)), ...)
  x$setInv(m)
  m
}
