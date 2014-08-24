## With both functions can calculates and stores cache the inverse of a matrix 

##makeCacheMatrix : This function creates a special "matrix" object that can
##cache its inverse


makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        list(set = set, get = get, 
             setsolve = setsolve,
             getsolve = getsolve)
}


##makeCacheMatrix : This function computes the inverse of a matrix and determine 
##if the inverse has already been calculated,
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
         inv <- x$getsolve()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}
