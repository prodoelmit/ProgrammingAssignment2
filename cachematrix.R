## Cached matrix inverse operation
## Steps:
## Create a matrix, e.g.
## > A <- matrix(c(2,4,3,1), nrow=2, ncol=2, byrow=T)
## Create a matrixWithCache object:
## > A.cached <- makeCacheMatrix(A)
## Find its inverse:
## > cacheSolve(A.cached)


## makeCacheMatrix(matrix):
## Creates matrix wrapper for cached inverse operation
makeCacheMatrix <- function(m_mtx = matrix()) {
	m_inv <- NULL
	set <- function(mtx) {
		m_mtx <<- mtx
		m_inv <<- NULL
	}
	get <- function() m_mtx
	setinv <- function(inv) m_inv <<- inv
	getinv <- function() m_inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve(x, ...)
## Finds inverse of cached matrix x with optional parameters
## x must be created with makeCacheMatrix  

cacheSolve <- function(x, ...) {
		invCached <- x$getinv()
		if (!is.null(invCached)) {
			message("getting cached data")
			return(invCached)
		}
		data <- x$get()
		inv <- solve(data, ...)
		x$setinv(inv)
		inv
}
