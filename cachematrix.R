## Pair of functions that cache the inverse of  
## a matrix

## makeCacheMatrix: This function creates a special "Matrix"
## object, which is really a list containing a function to:
## -set the value of the matrix
## -get the value of the matrix
## -set the value of the inverse matrix
## -set the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
      x <<- y
      m <<-NULL
    }
    get <- function() x
    setinv <- function(inverse) m <<- inverse
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv=getinv)
 }


## cacheSolve: this function calculates the inverse matrix of 
## the special matrix created with the makeCacheMatrix.
## Nevertheless, it first checks to see if the inverse matrix has been 
## computed. If so, it gets the inverse matrix from the cache and skips 
## the calculation. Otherwise, it calculates the inverse matrix by means 
## of the solve() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
