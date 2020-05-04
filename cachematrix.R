## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The makeCacheMatrix function
## 1.  sets the value of the matrix
## 2.  gets the value of the matrix
## 3.  set the value of the inverse result
## 4.  get the value of the cached inverse result


makeCacheMatrix <- function(x = matrix()) {
  inverseresult <- NULL
  set <- function(y) {
    x <<- y
    inverseresult <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inverseresult <<- inverse
  getinverse <- function() inverseresult
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## The following function calculates the inverse of the matrix
## created with the above function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it `get`s the inverse from the cache. 
## Otherwise, it calculates the inverse of the data and 
## sets the value of the inverse in the cache via the `setinverse` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverseresult <- x$getinverse()
  if(!is.null(inverseresult)) {
    message("getting cached data")
    return(inverseresult)
  }
  data <- x$get()
  inverseresult <- solve(data) %*% data
  x$setinverse(inverseresult)
  inverseresult
}
