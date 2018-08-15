## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#############################################################
## The below function uses lexical scoping in R to create 
## functions and symbol-value pairs specific to objects 
## of the makeCacheMatrix type. It defines and returns functions
## to set and retrieve the value of the above mentioned object 
## which is a matrix in this case and set and get its inverse


makeCacheMatrix <- function(x = matrix()) {
        i <- NA
        
        ## the below set function is useful to update the value of
        ## the cached object
        set <- function(y) {
            x <<- y
            i <<- NA
        }
        
        ## the get function retrives the value of the matrix object
        get <- function() x
        
        ## the setinverse function caches the inverse 
        setinverse <- function(inverse) i <<- inverse
        
        ## the getinverse function is used to retrieve the value 
        ## of the cached inverse
        getinverse <- function() i
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
#############################################################
## The below function accesses the cached value of the inverse
## of the matrix stored as an makecacheMatrix type object 
## and if such an inverse doesn't exist, it calculates the 
## inverse and caches it


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
        ## here, we check if there is an existing cached inverse
        i <- x$getinverse()
        if(!is.na(i)) {
            message("getting cached data")
            return(i)
        }
        
        ## if no cached inverse exists, we calculate one and 'set' it
        data <- x$get()
        print(data)
        i <- solve(data)
        x$setinverse(i)
        i
}


