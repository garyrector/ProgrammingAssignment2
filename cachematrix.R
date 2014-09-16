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
        ## This returns a list of 4 functions described below
        ## Argument is a numeric matrix but can be defaulted

        ## initialize variable to hold inverted matrix
        ## if "m" is a matrix, its inverse is obviously called "w"
        w <- NULL
        
        ## create the 4 management functions: set, get, setinv, getinv
        ## "set" creates the matrix to be inverted and nulls the cache
        set <- function(y) {
                m <<- y
                w <<- NULL
        }

        ## "get" returns the matrix to be inverted
        get <- function() m

        ## "setinv" caches the inverted matrix
        ## this should only be called by cacheSolve, not externally
        setinv <- function(solved) w <<- solved

        ## "getinv" can be used to return the inverted matrix AFTER caching
        getinv <- function() w
        
        # return a list containing the 4 functions
        list(set=set, get=get, setinv=setinv, getinv=getinv)    
}
##########################################################################

## Next, create a function to calculate or get the cached inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Arguments are the list created by makeCacheMatrix and any args for solve()

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
