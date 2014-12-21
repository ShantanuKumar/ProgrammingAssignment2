## In order to avoid costly computation, we are caching the inverse of matrix
## By using a pair of functions, we cache the inverse of matrix.


## Creates a special "matrix" object that can cache its inverse.
## "Matrix" object is really a list containing a function to:
##  1. set and get the value of matrix
##  2. set and get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) 
{
    x_inv <- NULL
    set <- function(y){
        x <<- y
        x_inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) x_inv <<- inverse
    getInverse <- function() x_inv
    
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve gets the inverse from cache and skips the computation.
cacheSolve <- function(x, ...) {
    x_inv <- x$getInverse()
    if(!is.null(x_inv)){
        message('Getting cached matrix inverse')
        return (x_inv)
    }
    m_data <- x$get()
    x_inv <- solve(m_data)
    x$setInverse(x_inv)
    x_inv
       
}
