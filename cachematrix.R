## two functions to cache inverse matrix operation

## > n <- 1000
## > m <- makeCacheMatrix(matrix(runif(n^2), n))
##
## > system.time({i <- cacheSolve(m)})
##    user  system elapsed 
##   2.929   0.038   2.965 
##
## > i[1,]
##    [1]  0.1163227551  0.0024698800  0.1211830392  0.0347113757  0.1023151509 ...
## 
## > system.time({i <- cacheSolve(m)})
## getting cached data
##    user  system elapsed 
##   0.001   0.000   0.001 
##
## > i[1,]
##    [1]  0.1163227551  0.0024698800  0.1211830392  0.0347113757  0.1023151509 ...  

## returns a list of functions accessing matrix 'i' captured in environment

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## calculates inverse matrix if not already stored in 'i' (see 'makeCacheMatrix')

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
