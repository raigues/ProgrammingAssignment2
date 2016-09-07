## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function returns the inverse of the matrix. It first checks to see
## if the inverse has already been computed. If so, it gets the result from the cache 
## and skips the computation. If not, it computes the inverse and sets the value in the 
## cache via setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInverse()
        
        if(!is.null(inv)){
                print("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
