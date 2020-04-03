## makeCacheMatrix and cacheSolve are a pair of functions that 
## create a special matrix object that stores its inverse in  
## in the cache

## Creates a list of functions that can be used to set and get the matrix
## object and the matrix inverse object.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        
        list(set = set, 
             get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Computes and stores the matrix inverse for a makeCacheMatrix object. 
## Returns the matrix inverse from the cache if it has been previously 
## calculated and no changes have been made to the matrix.

cacheSolve <- function(x, ...) {
                i <- x$getinverse()
                if(!is.null(i)) {
                        message("retrieved from cache")
                        return(i)
                }
                data <- x$get()
                i <- solve(data, ...)
                x$setinverse(i)
                i
        }

