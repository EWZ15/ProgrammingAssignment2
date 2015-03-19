#The following two functions are used to cache the inverse of a matrix, a potentially costly 
#computation time-wise if computed repeatedly.

#makeCacheMatrix sets and gets the value of the matrix and sets and gets the value of the 
#inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#This funciton (cacheSolve) checks to see if the inverse of the matrix has been computed already.
#If it has, it returns the result. If not, it runs the computation, caches the value, and prints
#the result.

#Assume that the matrix is always invertible...

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data...")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setinverse(inv)
  inv
}
