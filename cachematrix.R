## Put comments here that give an overall description of what your
## functions do
# The two functions below take a in a pre-defined matrix and calculates the inverse 
# of that matrix.  Before calculating the inverse of the matrix it first verifies it 
# does not have a cached value of that inverse.  If not it then calculates the inverse
# and caches the result so it is easier to retrieve later if necessary.  If there is a
# cached value already stored the functions will return the pre-calculated values.


## Write a short comment describing this function
# The makeCacheMatrix takes an input matrix "x" and creates a list of functions that
# set the value, get the value, set the value of the inverse, & get the value of the inverse.

# makeCacheMatrix <- function(x = matrix()) {
# 
# }
makeCacheMatrix <- function(x = matrix()) { 
    m <- NULL  
    set <- function(y){ 
        x<<- y   
        m <<- NULL  
    }
    get <- function() x 
    setinverse <- function(solve) m<<- solve    
    getinverse <- function() m  
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)    
}

## Write a short comment describing this function
# The cacheSolve function calculates the inverse of a matrix but first determines
# if it has already been calculated.  If so then it returns the previously cached value.
# if not then it calculates the inverse and saves it.
# cacheSolve <- function(x, ...) {
#         ## Return a matrix that is the inverse of 'x'
# }
cachSolve <- function(x, ...) {  
    m <- x$getinverse()  
    if(!is.null(m)) {   
        message("getting cached data")  
        return(m) 
    }
    data <- x$get() 
    m <- solve(data, ...)  
    x$setinverse(m)  
    m   
}