## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.

## The makeCacheMatrix function creates a special matrix, which is a list containing a function to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the inverse value of the matrix
## 4. Get the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## Define the four operations: set, get, setinverse, getinverse
        set <- function(y) {
                x <<- y         ## Use the superassignment operator '<<-' to assign 'x' in the global environment
                m <<- NULL      ## Ensure that the value of 'm' is NULL -- we check the NULL status in 'cacheSolve'
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}

## The cacheSolve function calculates the inverse of the special matrix created with makeCacheMatrix.
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from
## the cache and skips the computation. Otherwise, it calculates the inverse of the data using the 
## 'solve' function and sets the value of the inverse in the cache. It is assumed that the matrix
## supplied is always invertible.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {      ## If 'm' is not NULL, then return the cached matrix
                message("getting cached data")
                return(m)
        }
        data <- x$get()         ## Otherwise get the matrix and ...
        m <- solve(data, ...)   ## Calculate the inverse
        x$setinverse(m)         ## Set the inverse in the cache
        m                       ## Return a matrix that is the inverse of 'x'
}
