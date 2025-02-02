## makeCacheMatrix: This function creates a matrix object that can cache its inverse.
## It returns a list of functions to:
## 1. set the matrix value,
## 2. get the matrix value,
## 3. set the inverse of the matrix,
## 4. get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Function to set the matrix and reset the inverse cache
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse of the matrix
  getinverse <- function() inv
  
  # Return the list containing the above four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## It first checks if the inverse has already been computed. If so, it retrieves the inverse from the cache.
## Otherwise, it computes the inverse using solve, caches the result, and returns the inverse.

cacheSolve <- function(x, ...) {
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
