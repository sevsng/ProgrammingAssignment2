## These two functions assist in caching the inverse of a matrix.
## The first function creates a special "matrix" object that can 
## cache the inverse functions.  The second function is used to 
## compute the inverse of the "matrix" returned by the first function. 
## If the inverse has already calculated and the matrix has not 
## changed, it'll retrives the inverse from the cache directly.
## The two functions make it much easier and less intensive by 
## avoiding the recomputing the inverses with smilar results.

## This first function creates a special "matrix" object that can cache its inverse.
## There are four functions within this function which are
	## 1. set the matrix
	## 2. get the matrix
	## 3. set the inverse
	## 4. get the inverse
	## these elements are similar to our example.  Note: '<<-' assigns
	## a value to an object in the environment that different from
	## our current environment.  

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL 
    }
    get <- function (x)
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse
         getinverse = getinverse)
}


## This function is used to compute the inverse and cache the result
## of the makeCacheMatrix function above.  It does it in an optimal way
## by skipping the computations that have been calculuated already.
##

cacheSolve <- function(x, ...) {
    i <- x$getinverse
    if(!is.null(i)) {
        message("getting cache data")
        return(i)
    }
    data <- x$get()
    i <- inverse(data, ...)
    x$setinverse(i)
    i
}
