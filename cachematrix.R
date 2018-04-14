## The function makeCacheMatrix creates a list which can be used in the cacheSolve function.
## The pair of these functions can cache the inverse of a matrix


## The makeCacheMatrix function allows to set the value of the matrix, get the value of the matrix, set the
## value of the inverse of the matrix and get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
        
        set <- function(y) {
                
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)    

}


## The cacheSolve function compoutes the inverse of the special matrix returned by makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()
        if(!isnull(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get
        inv <- solve(x, ...)
        x$setinv(inv)
        inv
}
