## Background: These functions have been developed based on the sample code given.
## Purpose: Return the inverse of a matrix either by retrieving the result from
##   cache (if available) or by calculating it using the solve() function
## Input: a square matrix x
## Output: inverse of x
## Assumption: inverse of x exists

## Application Example
## > source("cachematrix.R")
## > x <- matrix(rnorm(9),nrow = 3)      #Generate a 3x3 matrix of normal random numbers
## > spmat <- makeCacheMatrix(x)         #Set Special Matrix spmat, which is a list of 4
## > spmat$get()                         #Get (display) spmat
##           [,1]       [,2]       [,3]
##[1,] -0.2357066 -0.6494716  0.9921604
##[2,] -0.5428883  0.7267507 -0.4295131
##[3,] -0.4333103  1.1519118  1.2383041
## > cacheSolve(spmat)                   #Calling cacheSolve for the first time, calculates
##           [,1]       [,2]      [,3]#inverse of spmat 
##[1,] -1.1678497 -1.6304219 0.3701890
##[2,] -0.7187575 -0.1155850 0.5357953
##[3,]  0.2599555 -0.4630001 0.4386789
## > cacheSolve(spmat)                   #Calling cacheSolve subsequently, gets result from 
## > getting cached data                 #cache
##           [,1]       [,2]      [,3]
##[1,] -1.1678497 -1.6304219 0.3701890
##[2,] -0.7187575 -0.1155850 0.5357953
##[3,]  0.2599555 -0.4630001 0.4386789



## The function makeCacheMatrix takes a matrix x and generates a list of 4 functions as follows:
## 1) Set the special matrix
## 2) Get the special matrix
## 3) Set the inverse i
## 4) Get the inverse i

makeCacheMatrix <- function(x = matrix()) {

  #Initialize the inverse matrix
  i <- NULL                
  
  #Set the special matrix
  set <- function(y) {     
    x <<- y
    i <<- NULL    
  }
  
  #Get the matrix
  get <- function() x      
  
  #Set the inverse
  setinverse <- function(inverse) i <<- inverse  
  
  #Get the inverse
  getinverse <- function() i   
    
  #Return the list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}



## This function returns the inverse of input matrix.
## If the inverse already exists, then it is retrieved from cache.
## Otherwise, it is calculated.

cacheSolve <- function(x, ...) {
          
  i <- x$getinverse()
    
  # If inverse already exists, then return it
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  #Otherwise, calculate it using the solve function
  data <- x$get()
  i <- solve(data, ...)
  
  #Set (cache) the inverse for subsequent retrieval
  x$setinverse(i)
  
  #Return the inverse
  i
  
  
}
