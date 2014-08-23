## Task description - below you may find functions are listed:
## (1.) makeCacheMatrix() serves as a helper function for cacheSolve().
##	  sub-functions setsolve and getsolve are called from cacheSolve()
## (2.) cacheSolve() computes the inverse of a square uncached matrix.
##      Additionally it caches them and the result may be returned faster
##	  in case of repetitive recall.


## First function (1.) - see above description
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
	  set <- function(y) {
                x <<- y
                m <<- NULL
        }
	  get <- function() x
	  
	  setsolve <- function(solve) m <<- solve
	  getsolve <- function () m
	  
	  list(set = set,
		 get = get,
		 setsolve = setsolve,
		 getsolve = getsolve
	  )
} 

## Second function (2.) - see above description
cacheSolve <- function(x, ...) {
	  m <- x$getsolve()
	  if(!is.null(m)) {
	  	   message("getting cached inv matrix data")
		   return(m)
	  }
	  MatrixData <- x$get()
	  m <- solve(MatrixData, ...)
	  x$setsolve(m)
	  m
}