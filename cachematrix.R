## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function

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


## Write a short comment describing this function
## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    x_inv <- x$getInverse()
    if(!is.null(x_inv)){
        message('Getting cached inverse')
        return (x_inv)
    }
    m_data <- x$get()
    x_inv <- solve(m_data)
    x$setInverse(x_inv)
    x_inv
       
}
