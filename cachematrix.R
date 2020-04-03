## makeCacheMatrix and cacheSolve are a pair of functions that 
## create a special matrix object that stores its inverse in  
## in the cache

## Creates a special matrix objected with a cache for the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the matrix inverse and stores it in the cache.
## Returns the matrix inverse from the cache if it has been previously 
## calculated and no changes have been made to the matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
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

