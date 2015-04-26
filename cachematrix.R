## The functions take a matrix and calculate the inverse.
## If the inverse had been calculated before, it only retrieves it from cache
## instead of calculating it again.

## This function get the matrix and creates other functions to manipulate it
## and assigns the inverse matrix to a variable
## The set function changes the matrix to the one of the argument and sets the inv value to NULL
## The get function only prints the matrix
## The setinverse assigns the value to inv to the one passed to the function (inver)
## The getinverse just gets the value of inv


makeCacheMatrix <- function(x = matrix()) {
inv<-NULL
set <- function(y) {
        x <<- y
        inv <<- NULL
}
get <- function() x
setinverse <- function(inver) inv <<- inver
getinverse <- function() inv
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
        
        
}


## This function calculates the inverse of the matrix
## First it uses the getinverse function to see if the inverse already exists
## If it exists then it gets the data stored in cache, if not then it continues and calculates 
## it using solve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        
}
