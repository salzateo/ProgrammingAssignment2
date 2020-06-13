##This function creates a list containing two functions to set and get the value of a given matrix,
##as well as two other functions to set and get the value of the inverse of that matrix. 
makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) Inv <<- inverse
  getinverse <- function() Inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##The next function will calculate the inverse of the matrix given before as long as it hasn't been 
##calculated yet, otherwise it'll jus get the result from the cache and ignore the calculation.
cacheSolve <- function(x, ...) {
  Inv <- x$getinverse()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data)
  x$setinverse(Inv)
  Inv
}