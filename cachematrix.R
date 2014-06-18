## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve # Sets the solve function for the inversion
        getinv <- function() m  # Performs the matrix inverse
        list(set = set, get = get, # returns the list of functions
             setinv = setinv,
             getinv = getinv)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data") # Returns message if inverse already exists
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) # Performs matrix inverse
        x$setinv(m)
        m
}
