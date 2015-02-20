
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL                            # m is assigned as NULL
        set <- function(y) {                 # defines a function to
        x <<- y                              # Reset the matrix, x, to a new matrix, y ( in global environment)
        m <<- NULL                           # and resets the inverse matrix, m, to NULL.( in global environment)  
                                             # This is to invalidate the cache. 
        }
        
        get <- function() x                        # returns the matrix , x
        setInverse <- function(solve) m <<- solve  # sets the Inverse of the matrix x.  Called by cacheSolve
        getInverse <- function() m                 # returns the value of the inverse matrix,m
        list( set = set,get = get,                 # each value in this list is a function that
             setInverse = setInverse,              # can be called upon. Example x$get().
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve will retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()                       # gets the value stored in getInverse which is the values 
                                                  # for inverse of the matrix. 
        if(!is.null(m)) {                         # if m is null, that means there is nothing in cache.
        message("getting cached data")            # generates a diagnostic message.
        return(m)                                 # returns the value of m which is the inverse matrix.
                                                  # Nothing below the return function will get executed.
        }
        data <- x$get()                          # if m is null, it gets the value of x from the matrix data 
                                                 # and assigns it to data
        m <- solve(data, ...)                    # calls the solve function which creates the inverse of the 
                                                 # matrix on data and assigns it to m
        x$setInverse(m)                          # m is being passed to setInverse. 
        m                                        # returns the value of m which is the inverse matrix
}

x <- makeCacheMatrix(rbind(c(2,4,0), c(2,0,0),c(2,0,1)))  
x$get()
cacheSolve(x)
cacheSolve(x)


