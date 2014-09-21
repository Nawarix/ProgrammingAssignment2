## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## create a list of setters and getters for caching the matrix and its inverse
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


## Write a short comment describing this function
## find the inverse of matrix if exists but 
## if it's created before it retrieve the cached inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
        if(!is.null(inv)) {
            message("getting cached inverse")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
