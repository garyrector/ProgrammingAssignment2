## This contains 2 functions: makeCacheMatrix and cacheSolve 
## These are used to manage the creation, computation, caching, 
##   and retrieval of the inverse of a given matrix
## Example usage:
##   > M<-makeCacheMatrix()
##   > M$set(matrix(rnorm(10000),100,100))
##   > m<-M$get()
##   > w<-cacheSolve(M)
##   > w<-cacheSolve(M)
##   getting cached data
##
## The call to makeCacheMatrix sets up the caching environment
## The call to M$set creates the matrix to be inverted
## The call to M$get returns the value of the matrix to be inverted
## The first call to cacheSolve above calculates the matrix inverse
## The second call to cacheSolve just gets the cached value of the inverse 
##########################################################################

## First, create a list of functions to manage the matrix and its inverse
makeCacheMatrix <- function(m = matrix(numeric())) {
        # initialize variable to hold inverted matrix
        # if "m" is a matrix, its inverse is obviously called "w"
        w <- NULL
        
        # create the 4 management functions
        set <- function(y) {
                m <<- y
                w <<- NULL
        }
        get <- function() m
        setinv <- function(solved) w <<- solved
        getinv <- function() w
        
        # return a list containing the 4 functions
        list(set=set, get=get, setinv=setinv, getinv=getinv)    
}
##########################################################################

## Next, create a function to calculate or get the cached inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Initialize returned variable with (possibly) 
        ##   pre-calculated inverted matrix
        w <- x$getinv()
        if(!is.null(w)) {
                ## if here, matrix was previously inverted and cached 
                message("getting cached data")
                return(w)
        }

        # if here, matrix was not previously inverted and cached, so:
        # get the matrix to be inverted and calculate the inverse
        data <-x$get()
        w <- solve(data,...)
        
        # then cache the inverted matrix
        x$setinv(w)
        
        # finally, return the inverted matrix
        w
}
##########################################################################
