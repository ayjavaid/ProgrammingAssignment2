## The following functions are being used to create a matrix and store its inverse.

## The first function, `makeCacheMatrix` creates a matrix, which is
## actually a list of functions to perform the following operations:
##
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of this matrix
## 4.  get the value of the inverse of this matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  
  # Point 1 above
  set <- function(y){
    x <<- y
    inv_mat <<- NULL
  }
  
  # Point 2 above
  get <- function() x
  
  # Point 3 above
  setinverse <- function(solve) inv_mat <<- solve
  
  # Point 4 above
  getinverse <- function() inv_mat
  
  # Lists all possible operations in this function
  list(set=set, get=get, setinverse=setinverse,
       getinverse=getinverse)

}

## The following function calculates the inverse of the matrix created with 
## the above function 'makeCacheMatrix'. It performs following operations:
##
## a. It first checks to see if the inverse has already been calculated and present.
## b. If so, it fetchse the stored inverse from the cache and skips the computation 
##    using the function 'getinverse'. 
## c. Otherwise, it calculates the inverse of the matrix and sets the value of the
##    inverse in the cache via the `setinverse` function.

cacheSolve <- function(x, ...) {
  
  inv_mat <- x$getinverse()   
  
  # Operation a above
  if(!is.null(inv_mat)){
    message("Fetching cached matrix inverse.")
    return(inv_mat)
  }
  
  # Operation b above
  matrix <- x$get()
  
  # Operation c above
  inv_mat <- solve(matrix, ...)
  x$setinverse(inv_mat)
  inv_mat
}
