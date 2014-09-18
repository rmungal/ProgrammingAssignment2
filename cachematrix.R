## Two functions: write a pair of functions that cache the inverse of a matrix
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function( m = matrix() ) {
	## Start the inverse 
    i <- NULL
    ## method to set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }
    ## method to get the matrix
    get <- function() {
    	## Return the matrix
    	m
    }
    ## method to set the inverse
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## method to set the inverse
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## Final list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}




## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed) 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## Return an inverse if already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Using matrix multiplication calculate the inverse 
    m <- solve(data) %*% data

    ## Set the inverse of the object
    x$setInverse(m)

    ## Return the matrix
    m
}

