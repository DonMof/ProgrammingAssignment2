## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special matrix that sets and gets the value of a matrix
## and sets and gets the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  
          invm <- NULL
          set <- function (y){
          x <<- y
          invm <<- NULL
        }
  get <- function() x
  setinverse <- function(invm) invm <<- solve(x) #calculate matrix inverse
  getinverse <- function() invm
  list (set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve caches the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invm <- x$getinverse()
  if(!is.null(invm)){
      message("getting cached data")
      return(invm)
  }
  mydata <- x$get()
  print("mydata")
  invm <- solve(mydata, ...)
  x$setinverse(invm)
  invm
}
