## Creates a list of functions for matrix storage and retrieval operations

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
		## sets the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x ## retrieves the matrix
        setsolve <- function(solve) m <<- solve  ## sets the matrix inverse 
        getsolve <- function() m                 ## retrieves the existing matrix inverse
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
		 m <- x$getsolve()
        if(!is.null(m)) { ## if the matrix inverse exists, retrieve
                message("getting cached data")
                return(m) ## exit function, returning the existing inverse
        }
        data <- x$get() ## gets the matrix
        m <- solve(data, ...) ## solves the matrix inverse
        x$setsolve(m)  ## stores the matrix inverse
        m ## display matrix inverse
}
