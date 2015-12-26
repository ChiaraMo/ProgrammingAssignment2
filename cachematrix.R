## Cache the inverse of a matrix

## Creates a special "matrix" that is actually a list containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv

        ## stores the four functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calcuates the inverse of the special "matrix". 
## It first checks to see if the inverse of the matrix has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation
## Otherwise, it calculates the inverse and sets the value of the inverse in the cache via 
## the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getinverse() ## get the inverse of the matrix from the cache
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## computes the inverse of the given matrix in case it was not calculated yet
        data <- x$get()
        inv <- solve(data, ...)
        
        ## stores the calculated inverse of the matrix in the cache
        x$setinverse(inv)
        
        ## returns the calculated inverse of the matrix
        inv
}
