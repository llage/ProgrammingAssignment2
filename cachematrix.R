## With these functions one can create a special matrix object that caches
## its inverse. The inverse will be retrieved from cache if it exists there.
## This may be useful in loops in order to avoid unnecessarily repeating
## calculations.
## Code is part of a programming assignment and derived from the code given
## in the assignment, for obvious reasons.
## Example usage:
## > mymatrix <- makeCacheMatrix (matrix(c(1,4,2,3),nrow=2,ncol=2))
## > cacheSolve(mymatrix)
## When the last command is repeated, the inverse will come from cache.

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	solution <- NULL			## initialize
	set <- function(y) {
		x <<- y				## input arg to x in parent
		solution <<- NULL		## clear previous solution
	}
	get <- function() {
		x				## getter for x
	}
	setsolution <- function(z) {
		solution <<- z			## setter for inverse
	}
	getsolution <- function() {
		solution			## getter for inverse
	}
	
	list ( set = set, get = get,		## functions to parent env
		   setsolution = setsolution, getsolution = getsolution )
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	solution <- x$getsolution()		## try to get cached inverse
	if(!is.null(solution)) {		## ie if it's already cached
		message("getting cached inverse")       
		return (solution)		## return cached inverse
	}					## else (if not yet cached):
	solution<-solve(x$get(), ...)		## get values & calc inverse				
	x$setsolution(solution)			## cache inverse now
	return(solution)			## return inverse
}
