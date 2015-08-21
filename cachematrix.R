makeCacheMatrix <- function(x = matrix()) {

  m <- NULL                                 ##begins by setting the mean to NULL as a placeholder for a future value

  set <- function(y) {x <<- y; m <<- NULL}  ##defines a function to set the vector, x, to a new vector, y, and resets the mean, m, to NULL

  get<-function() x                         ##returns the vector, x

  setmatrix<-function(solve) m<<- solve     ##sets the mean to matrix, m

  getmatrix<-function() m                   ##returns the matrix, m

  list(set=set, get=get,                    ##returns the 'special vector' containing all of the functions just defined
     setmatrix=setmatrix,
     getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
  
  m<-x$getmatrix()                          ##get the inversed matrix from object x
                                            ##it will be null if uncalculated ("m <- NULL" in the previous function)
  if(!is.null(m)){                          ##if the inversion result is there
    message("getting cached data")
    return(m)                               ##return the calculated inversion
  }

  matrix<-x$get()                           ##if not, we do x$get to get the matrix object
  
  m<-solve(matrix, ...)                     ##we solve it
  
  x$setmatrix(m)                            ##we then set it to the object
  
  m                                         ##return the solved result
}
