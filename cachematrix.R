## These functions work very similar to the makeVector example function and the cachemean 
## example function.  

## This function creates a special "matrix" object that can cache its inverse.

## The makeCacheMatrix function first creates and empty NA function with 
## matrix(,,) - no data, no rows, no columns.  This can also be done by 
## replacing the matrix(,,) with NULL and the is.na in cacheSolve to is.null

## if the matrix has been calculated previously, then the function will "get" it

## if the matrix inverse been calculated previously and stored in cache, then the function will "get" it

makeCacheMatrix <- function(x = matrix()) {
        
##call function using yourmatrixname <- makeCacheMatrix(matrix( numbers, nrows, ncol))        
        
        matrix_i <- matrix(,,)
        set <- function(y) {
                x <<- y
                matrix_i <- matrix(,,)
        }
        get <- function() x
        setinverse <- function(solve) matrix_i <<- solve
        getinverse <- function() matrix_i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)                
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## IT calls the previously calculation inverse matrix from cache (if the matrix has not changed)
## or calculates the inverse for the first time if needed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
                matrix_i <- x$getinverse()
                if (!is.na(matrix_i)) {
                        message("getting cached data")
                        return(matrix_i)
                }
                data <- x$get()
                matrix_i <- solve(data, ...)
                x$setinverse(matrix_i)
                matrix_i
                
}