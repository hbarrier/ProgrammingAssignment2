## This function allows to calculate the inverse of a matrix and cache the result for efficient retrieval and reuse


## The makeCacheMatrix creates a matrix object with 4 functions, 
## enabling in particular to retrieve the cached inverse after it has been calculated

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL  #initializes the Inverse matrix to null to avoid error 
  set <- function(y) {
    x <<- y
    I <<- NULL  ## in case the matrix is updated, reinitializes the inverse to avoid a previous Inverse matrix is retrieved
  }
  get <- function() x
  setInverse <- function(m_Inverse) I<<-m_Inverse
  getInverse <- function() I
  list(set=set, get=get,setInverse=setInverse,getInverse=getInverse)
}


## the cacheSolve function checks if the inverse of the matrix given in argument has already been cached
## if so, it returns it
## if not, it calculates it & stores it for future reuse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  I <- x$getInverse()
  if (!is.null(I)) {
    message("retrieving the cached matrix")
    return(I)
  } else {
    message("caching the matrix for reuse")
    data <- x$get()
    I <- solve(data)
    x$setInverse(I)
    return(I)
  }
  
}