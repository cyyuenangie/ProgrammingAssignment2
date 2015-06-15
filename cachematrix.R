## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { #Make CacheMAtrix is similar to makeVector 
 inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()    #store inverse value from makeCacheMatrix
  if(!is.null(inv)) {  #if inverse exists, getting cached data
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)  #if inverse is null, calcuate inverse of x with solve function
  x$setinv(inv) #set inverse value
  inv   #return inverse
}
