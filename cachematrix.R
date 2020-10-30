## Here 2 functions are created: makeCacheMatrix and cacheSolve

## this creates a special "matrix" that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## this computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)){
    message("Getting cache data")
    return(s)
  }
  data <- x$get()
  s <- solve(data,...)
  x$setinverse(s)
  s
}
