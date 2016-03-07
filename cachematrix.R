## The <<- operator  can be used to assign a value to an object 
# in an environment that is different from the current environment. Below are two functions that are
# used to create a special object that stores a numeric matrix and cache's its inverse.

#The first function creates a special "matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse matrix
#get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()){
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) i <<- solve
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse matrix in the cache via the setmean function.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
        return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
## Return a matrix that is the inverse of 'x'
}
