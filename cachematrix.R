## Matrix inversion is a costly computation. 
## These two functions enable you to calculate and store an inversed matrix in the cache. 
## The inversed matrix only needs to be calculated when its content is changed. 
## Otherwise the cached matrix will be retuned.


## This function lets you create a place for the cached inversed matrix.
## The function also creates 4 functions to access and modify chached matrix.
## Because of the lexical scoping of R, no matter where these 4 functions are used,
## they will have access to the cached matrix 
## because both the matrix and the functions are created in the same environment.

makeCacheMatrix <- function(x = matrix()) {
        
        ## placeholder for the inversed matrix
        inverse <- NULL
        
        ## function to set the original matrix
        ## the special assigment operator <<- forces the calling function to search for x in parent environments 
        set <- function(y) {
                x <<- y
                ## when a new matrix is set, make sure the current stored inversed matrix is removed        
                inverse <<- NULL
        }
        
        ## function that just returns the original matrix
        get <- function() x
        
        ## function to set the inversed matrix. The inversed matrix will be calculated in the calling environment
        ## and then passed to the parent environment, so <<- is used.
        setinversed <- function(inv) inverse <<- inv
        
        ## function that just returns the inversed matrix
        getinversed <- function() inverse
        
        ## the 4 functions are put in a list and tagged with their own name. 
        ## This list is returned.
        list(set = set, get = get,
             setinversed = setinversed,
             getinversed = getinversed)
}


## This function gets the cached inversed matrix. If there is no cached inversed matrix, it will be calculated and set.
## Note that before using this function two steps are required.
## First the cache needs to be set up with invoking makeCacheMatrix().  
## Second, a matrix needs to be set in the cache with set()

cacheSolve <- function(x, ...) {
        
        ## get the chached inversed matrix
        inverse <- x$getinversed()
        
        ## if there is a chached inversed matrix, print a line and return it and quit the function 
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        ## there is no chached inverse matrix
        ## so get the original matrix (stored in cache by a previous set() command)
        data <- x$get()
        
        ## calculate the inverse of the original matrix
        inverse <- solve(data, ...)
        
        ## and store it in the cache
        x$setinversed(inverse)
        
        ## return the inversed matrix that was just calculated
        inverse
}
