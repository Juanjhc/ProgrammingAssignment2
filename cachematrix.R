## The functions makeCacheMatrix and cacheSolve allow you to cache the inverse of a matrix
## then result will be evaluated if same inverse is needed for other calculations

## makeCacheMatrix to create a special object as matrix will be stored in the cache

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y 
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(solve)j <<- solve
  getInverse <- function()j
  list (set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve to compute inverse of matrix or retrieve the inverse if it has 
## already been calculated and matrix has not changed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)) {
    message("getting cached data")
    return(j)
  }
  matrix_inv <- x$get()
  j <- solve(matrix_inv,...)
  x$setInverse(j)
  j
}
