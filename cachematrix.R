## The pair of functions allow you to make a special matrix object that can
## cache the result of a its inverse calculation. 
## The matrix inverse is calculated using the solve function and it is assumed
## that the matrix supplied is always invertible.


## This function adds new methods to matrix object being passed 
## and returns a special form of the matrix object with new methods

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## This function uses the methods in the cachematrix object and checks
## to see if the matrix inverse has already been calculated and cached
## It returns the cached inverse or calculates and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
