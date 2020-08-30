## A couple of functions to calculate the inverse of a matrix passed as input.

makeCacheMatrix <- function(x = matrix()){.  #initializing a function that creates a list of functions to use during execution of next function
  m <- NULL. 
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) #this function returns the list of functions with the matrix variable to calculate the inverse
}

cacheSolve <- function(x, ...){ #initializing a function that uses a list of functions created during execution of previous function to calculate inverse of the matrix
  m <- x$getinverse()
  if(!is.null(m)) {    #if m already has cached values the function returns m, else it continues calculation
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
} # inverse of the matrix is calculated and returned

## USAGE
mat1 <- matrix(rnorm(9), nrows = 3, ncols = 3) #initialize any matrtix
cacheSolve(makeCacheMatrix(mat1)) #pass the matrix through the function to calculate the inverse



