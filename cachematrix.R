#Make function that save time computing by cache the data related
#to the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y){
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) mat <<- solve
  getmatrix <- function() mat
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


# The function  checks if the inverse has already been computed. 
#If TRUE , return "From the cache" 
# Else : computes the inverse and sets the value in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat <- x$getmatrix()
  if(!is.null(mat)){
    message("From the cache")
    return(mat)
  }
  matrix <- x$get()
  mat <- solve(matrix, ...)
  x$setmatrix(mat)
  mat
}
