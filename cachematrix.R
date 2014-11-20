## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  I <- NULL #I is inverse and is set to NULL at the beginning
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x #returns input matrix
  setInv <- function(solve) I <<- solve #finds the inverse and caches it
  getInv <- function() I #returns cached value
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv) #list of functions created in cachematrix

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { # is the input matrix of makeCacheMatrix
        ## Return a matrix that is the inverse of 'x'
  I <- x$getInv()#returns inverse of x
  if(!is.null(I)) {
    message("getting cached data") #return inverse if it's cached
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)#calculates inverse if it's not cached
  x$setInv(I)
  I #returns inverse
}
