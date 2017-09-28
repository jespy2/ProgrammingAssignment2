## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Creates makeCacheMatrix function which creates a matrix object
makeCacheMatrix <- function(x = matrix()) {
  invmtx <- NULL          ##Creates invmtx which will hold the value for the matrix inverse
  setmtx <- function(y) { ##Creates setmtx function which assigns value of matrix...
    x <<- y               ##...in parent environment
    invmtx <<- NULL
  }
  getmtx <<- function() x ##Creates getmtx function & returns value of matrix
  setinv <<- function(inverse) invmtx <<- inverse ##Sets value of invmtx in parent envir
  getinv <<- function() invmtx  ##Get the value of invmtx
  ##Create a list so that we can use the $ operator
  list(setmtx=setmtx, getmtx=getmtx, setinv=setinv, getinv=getinv) 
}


## Write a short comment describing this function
##Creates cacheSolve which computes the inverse of the matrix returned by the makeCacheMatrix 
##function.  If the inverse has already been calculated (and the matrix has not changed),
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmtx <- x$getinv()
  if(!is.null(invmtx)){              ##if inverse matrix is not NULL
    message("Getting cached data")  ##prints "Getting cached data"
    return(invmtx)                  ##returns matrix for inverting
  }
  mtxdata <- x$getmtx()             ##retrieve original matrix data
  invmtx <- solve(mtxdata,...)      ##inverse matrix with solve
  x$setinv(invmtx)                  ##set inverse
  return(invmtx)                    ##return inverse
}
