## Put comments here that give an overall description of what your
## functions do

# 1. makeCacheMatrix:
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        # initiative value is NULL, it will hold matrix inverse
        inv <- NULL
        # define function set to assign new value of
        set <- function(y) {
                # matrix in parent environment
                x <<- y
                # if there is a new matrix, reset inv to NULL
                inv <<- NULL
        }
        # actually this getter function seems useless...
        get <- function() x
        # saves computed inverse value in parent environment
        setinverse <- function(inverse) inv <<- inverse
        # gets the inverse value when called
        getinverse <- function() inv
        list(
                set = set, get = get, setinverse = setinverse,
                getinverse = getinverse
        )
}

# 2. cacheSolve:
# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated (and the
# matrix has not changed), then the cachesolve should retrieve the inverse from
# the cache.
cacheSolve <- function(x, ...) {
        # try to get the possibly cached inverse of x from saved resource
        inv <- makeCacheMatrix(x)$getinverse()
        if (!is.null(inv)) {
                message("cached inverse got")
                return(inv)
        }
        # if there is no cached data then compute the inverse
        inv <- solve(x, ...)
        # save the computed inverse into its cache using makeCacheMatrix()
        # function for possible, later invocation
        makeCacheMatrix(x)$setinverse(inv)
        inv
}

# cells <- c(11, 126, 124, 168)
# rnames <- c("R1", "R2")
# cnames <- c("C1", "C2")
# mymatrix1 <- matrix(cells,
#         nrow = 2, ncol = 2, byrow = TRUE,
#         dimnames = list(rnames, cnames)
# )