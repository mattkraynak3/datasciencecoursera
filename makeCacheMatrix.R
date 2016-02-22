## Matt Kraynak: Programming Assignment 2: Week 3- Introduction to R Programming
##
## The makeCacheMatrix function creates a list  
## containing a function that will
## 1. set the value of the matrix 
## 2. get the value of the matrix 
## 3. set the value of the inverse of the matrix 
## 4. get the value of the inverse of the matrix 
## 
 
makeCacheMatrix <- function(x = matrix()) { 
    i <- NULL 
    set <- function(y) { 
      x <<- y 
      i <<- NULL 
    } 
    get <- function() x 
    setinverse <- function(inverse) i <<- inverse 
    getinverse <- function() i 
    list( 
      set = set, 
      get = get, 
      setinverse = setinverse, 
      getinverse = getinverse) 
  } 
  
## The cacheSolve function computes the inverse of the special matrix 

## This checks if the inverse has already been calculated. If it has, 
## it gets the inverse from the cache and skips over the computation. 
## Else it will calculates the inverse of the matrix and sets the value  
## of the inverse in the cache by using the setinverse function. 
  
cacheSolve <- function(x, ...) { 
  ## Return a matrix that is the inverse of 'x' 
  i <- x$getinverse() 
  if(!is.null(i)) { 
    message("getting cached data") 
    return(i) 
   } 
  data <- x$get() 
  i <- solve(data, ...) 
   x$setinverse(i) 
   i 
} 

