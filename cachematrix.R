## These 2 functions will cache a matrix, and, if unchanged, retrieve 
## the already calculated inverse of that matrix quickly.

## This first function creates a matrix that can "cache" its inverse

makeCacheMatrix <- function(x = matrix()) {
		inv<- NULL
		set <- function(y){
			x <<- y
			inv <<- NULL
		}
		get <- function() {
		        x
		      }
		setInverse <- function(solveMatrix) inv <<- solveMatrix
		getInverse <- function() inv
		list(set = set, get = get, setInverse = setInverse, getInverse 			= getInverse)
}


## If above matrix is calculated and unchanged, this function should
## retrieve the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        ## return matrix inverse if already previously computed
        if (!is.null(inv)){
        	message ("getting cached data")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
