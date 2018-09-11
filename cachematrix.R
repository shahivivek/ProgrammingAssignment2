## makeCacheMatrix is a class which has matrix and 4 functions to set, get, setinverse and getinverse function.
## Set - set matrix
## Get - get matrix
## setinverse - calculate inverse of the matrix
## getinverse - get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(solve) m<-solve
  getinverse<-function() m
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## cacheSolve function return the inverse of the given matrix. 
## First it checks if inverse of the matrix is already available in cache and if it is available it return from there.
## otherwise it calculate the inverse by calling setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m      ## Return a matrix that is the inverse of 'x'
}
