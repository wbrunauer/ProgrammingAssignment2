## Overall description
# The following two functions allow computation 
# and caching the results of a matrix inversion.

## Description of function makeCacheMatrix()
# The first function creates a special "matrix", 
# which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the matrix inverse
# 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Description of function cacheSolve()
# The following function calculates the inverse 
# of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data 
# and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

## Example
# Create matrix
#mat <- matrix(c(3,1,2,1),nrow=2)
# Check result
#(matinv <- solve(mat))
#round(matinv%*%mat,2) # Identity


# Create special "matrix" list
#x <- makeCacheMatrix(mat)
# Use function for solving
#cacheSolve(x)
# If used again, the results are called from the cache
#cacheSolve(x)
