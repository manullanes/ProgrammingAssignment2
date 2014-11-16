## Put comments here that give an overall description of what your
## functions do

##  makeVector creates a special "matrix"
##  which is really a list containing a function to:
#   1. set the value of the vector
#   2. get the value of the vector
#   3. set the value of the inverse matrix
#   4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
        x<<-y
        m<<-NULL
}
   get<-function() x
   setsolve<-function(solve) m<<-solve
   getsolve <- function() m
  list(set=set,get=get,
       setsolve=setsolve,
       getsolve=getsolve)
}

## First checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
       message("getting cached data")
       return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
