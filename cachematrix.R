## Creates a special matrix object that can cache the input matrix, and its inverse
## Setting and Obtaining(get) the value of the matrix
## Setting and obtaining(get) the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  b<-NULL # Default in case cacheSolve has not been called
  set<-function(y) {
     x<<-y
     b<<-NULL
  }
get<-function() x
setmatrix<-function(solve) b<<-solve
getmatrix<-function() b
list(set=set, get=get,
     setmatrix=setmatrix,
     getmatrix=getmatrix)
}


## computes the inverse of the matrix returned by makeCacheMatrix
## Will retrieve the inverse from cache, if the inverse has already been called
## and the matrix remains unchanged.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    return(m)   ## Obtains cached data
  }
  matrix<-x$get()
  m<-solve(matrix,...)
  x$setmatrix(m)
  m
}
