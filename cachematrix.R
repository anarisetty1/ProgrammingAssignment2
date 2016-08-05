#Put comments here that give an overall description of what your functions do

#If we pass an invertible matrix, the below two functions calculate the inverse of a matrix
#or inverse matrix is retrieved from the cache

## Write a short comment describing this function

#makeCacheMatrix has 4 functions, set and get functions sets and gets the value of a
#matrix respectively. setInverse and getInversefunctions sets and gets the Inverse of a
#matrix respectively.
#If we want to change the matrix we are sending in, we need to use the set function
#setInverse and getInverse only stores and returns the value of Inverse.
#They dont actually calculate the Inverse.
#The new Inverse needs to be recalculated through the function cacheSolve.
makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) I <<- Inverse
  getInverse <- function() I
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Write a short comment describing this function
##Input of cachesolve is the object where makeCacheMatrix is stored. 
##cacheSolve verifies whether the value I, previously stored with getInverse, exists
##and not null.If it exists in memory, it simply returns a message and the value I
#Otherwise, data gets the matrix stored with the makeCacheMatrix.
# I calculates the inverse of the matrix and x$setInverse(I) stores it in the object 
#generated assigned with makeCacheMatrix.
cacheSolve <- function(x, ...) {
  I <- x$getInverse()
  if(!is.null(I)){
    message("getting cached data")
    return(I)
  }else {
    data <- x$get()
    I <- solve(data)
    x$setInverse(I)
    I
  }
        ## Return a matrix that is the inverse of 'x'
}





