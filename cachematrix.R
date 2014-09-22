## Step1: make an object to get the matrix cache
## Step2: calculate the inverse of the square matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(inverse) iv <<- inverse
  getinvmatrix <- function() iv
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  iv <- x$getinvmatrix()
  
  ## Distinguish whether the matrix is reversible or not
  data <- x$get()
  if (dim(data)[1]-dim(data)[2]!=0){
    message("This is not a square matrix!")    
  } else if (det(data)==0){
    message("The matrix is irreversible!")
  } else if (!is.null(iv)){
      iv <- solve(data, ...)
      x$setinvmatrix(iv)
      message("getting cached data")
      iv
  }
}
