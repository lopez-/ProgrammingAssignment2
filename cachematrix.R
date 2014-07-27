# in this file there are two functions: makeCacheMatrix and cacheSolve and they
# can be used to return the inverse of a given matrix

# makeCacheMatrix is a function that takes a matrix as a unique parameter
# and it is used to create a matrix object that can cache its inverse
# there are four functions within makeCacheMatrix:
# 1. set: it can be used to set another matrix different from what has been
# previously set
# 2. get: returns the matrix stored within
# 3. setInv: it allows to set the inverse of a matrix
# 4. getInv: returns the inverse only if it has been calculated in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInv <- function(inv) i <<- inv
        getInv <- function() i
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


# cacheSolve is a function that returns the inverse of a matrix stored in
# makeCacheMatrix, and there are two possible options:
# 1. in case the inverse has been already calculated it retrieves the inverse 
# from the cache
# 2. in case the inverse has not been calculated (is.null(i)=TRUE) it will 
# calculate through the butil-in function solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInv(i)
        i
}
