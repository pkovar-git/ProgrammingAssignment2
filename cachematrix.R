## Put comments here that give an overall description of what your
## functions do
## Functions provide an interface for creating regular matrix stored in different environment 
## and computing of invertible matrix. Once the invertible matrix is computed it is cached.  

## Write a short comment describing this function
## Create a matrix with interface for computing invertible matrix and caching the result
makeCacheMatrix <- function(x = matrix()) {
	im <- NULL
	set <- function(y) {
                x <<- y
                im <<- NULL
      }
      get <- function() x
      setim <- function(solve) im <<- solve
      getim <- function() im
      list(set = set, 
	     get = get,
           setim = setim ,
           getim = getim )
}


## Write a short comment describing this function
## Function returns invertible matrix to regular matrix x (created with makeCacheMatrix)
## The function stores invertible matrix after it's computed.
## The function returns stored result if called with the same input second and other times. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getim()
        if(!is.null(im)) {
                message("getting cached matrix")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setim(im)
        im
}
