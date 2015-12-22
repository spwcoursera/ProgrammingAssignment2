## make a function creates a special "matrix" object that can cache its inverse.
## this function is a list containing a function to 
## 1. set a matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverst of the matrix

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                x <<- y
                m <<- NULL
                }
                get <- function() x
                setinversematrix <- function(inverse = matrix()) m <<- inverse
                getinversematrix<- function() m
                list(set = set, get = get,
                        setinversematrix = setinversematrix,
                        getinversematrix = getinversematrix)
        

}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
                ## Return a matrix that is the inverse of 'x'
                m <- x$getinversematrix()
                if(!is.null(m)) {
                         message("getting cached data")
                         return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setinversematrix(m)
                m
}
