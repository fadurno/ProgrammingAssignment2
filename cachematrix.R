## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix function creates a special "matrix", 
## which is a list containing a function to get and set both 
## the normal matrix and the inverse matrix
makeCacheMatrix <- function(x = matr
makeCacheMatrix <- function(x = matrix()) {
    #create a variable for the stored inverse value and initialize it to NULL
    inv <- NULL
    # function to set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL   #invalidate the variable
    }
    #get the value of the matrix
    get <- function() x
    #set the inverse
    setinv <- function(inv_) inv <<- inv_
    #get the inverse
    getinv <- function() inv

    # return a list of all the above functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)     

}


## Write a short comment describing this function

## The following function calculates the inverse a matrix assuming
## that the inverse exists. Before calculating the inverse it checks
## if the value has already been calculated and stored in the cache. 
## In this case it gets the inverse from the cache and skips the 
## calucluation, othwerise the normal process is applied.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # check if the inverse is already cached
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    #the result is not in cache, the matrix is added to the data varaiable
    data <- x$get()
    #we have now to calculate the inverse
    inv <- solve(data, ...)
    #and add it to the cache
    x$setinv(inv)
    #finally we return the value
    inv
}
