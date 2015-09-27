## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Matrix receives a matrix m when it is called.  It allows you to:
# get the matrix
# set the matrix value if you wanted to change it.
# get the inverse of the matrix
# set the inverse of the matrix 

makeCacheMatrix <- function(m = matrix()) {
        #makeCache Matrix receives a matrix m when it is called.
        
        #set inverse to NULL in beginning
        i <- NULL 
        
        #set function to set matrix for which we will later find inverse
        set <- function(y) {  
                m <<- y
                i <- NULL
                
        }
        
        #get function to retrieve matrix we provided during function call.
        get <- function() m
        
        #setinverse function - ensures the inverse i has the inverse 
        #calculated and cached in cacheSolve()
        setinverse <- function(inverse) i <<- inverse
        
        #retrieves and prints the inverse
        getinverse <- function() i
        
        #Ensures when we assign makeCacheMatrix to an object, the object 
        #has all the 4 functions.
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
        
}


## Write a short comment describing this function
# cacheSolve returns the matrix of x.  If it already cached, the cached value w
# will be returned, otherwise the value will be calculated, cached and then 
# returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #function that returns the inverse of the matrix or the cached 
        #value of the matrix
        
        #find out what is currently stored in the makeCacheMatrix function
        #as the inverse value
        i <- x$getinverse()
        
        #if inverse exists, then return the cached value.  Otherwise, continue
        #the statements in the if clause
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        
        #get the matrix that was passed to the makeCacheMatrix() function
        m <- x$get()
        
        #compute and cache the inverse of the matrix
        i <- solve(m)
        
        #set the inverse
        x$setinverse(i)
        
        #return the inverse
        i
}
