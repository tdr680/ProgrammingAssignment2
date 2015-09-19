## Put comments here that give an overall description of what your
## functions do

## > n <- 1000
## > m <- makeCacheMatrix(matrix(runif(n^2), n))
## > i <- cacheSolve(m)
## > i[1,]
##    [1]  0.1163227551  0.0024698800  0.1211830392  0.0347113757  0.1023151509 ...
## 
## > i <- cacheSolve(m)
## getting cached data
## > i[1,]
##    [1]  0.1163227551  0.0024698800  0.1211830392  0.0347113757  0.1023151509 ...  

## Write a short comment describing this function

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


## Write a short comment describing this function

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
