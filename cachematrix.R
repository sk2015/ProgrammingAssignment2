## Below functions will cache the inverse of a matrix passed to the function


## makeCacheMatrix : This function creates a special matrix which is a list of functions to 
##                   1.Set the matrix
##                   2.Get the matrix
##                   3.Set inverse of the matrix
##                   4.Get inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  #invr is a variable for inverse, initially set to NULL
   invr <- NULL
  
  #Set the matrix
   set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  
  #Get the matrix
   get <- function() x
  
  #Set inverse of the matrix
   setinvr <- function(inverse){
    invr <<- inverse
  }
  
  #get inverse of the matrix
   getinvr <- function(){
     invr
   }
  
  #return a list of functions
  list(set=set, get=get, setinvr=setinvr, getinvr=getinvr)
  
}


## cacheSolve: This function computes the inverse of the special matrix created by 'makeCacheMatrix' function. 
##             If the inverse is already computed, then 'CacheSolve' should return the inverse from the cache.

cacheSolve <- function(x, ...) {
     
   #get inverse of the matrix 'x'
    invr <- x$getinvr()
    
   #if inverse of matrix is already cached, return the cache
    if(!is.null(invr)) {
        message("getting cached data")
        return(invr)
    }
   
   #if inverse is not cached, return the inverse of the matrix and set in the cache
   
   #get the matrix from the object
    mat.data <- x$get()
   
   #calculate the inverse using matrix multiplication
    invr <- solve(mat.data,...)
   
   #Set the inverse to the object
    x$setinvr(invr)
   
   #Return the matrix
    invr
}
