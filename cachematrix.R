## The first function sets up a series of variables and functions in the enviroment
## that enable you to store and draw down the output of large calculations
## The second function provide the output of inversing a matrix; either by calculating
## it or pulling down the inverse from a stored variable in the environment.

## This function sets up the functions and variables required to cache the inverse
## of a matrix. It creates a set/get functions pairs which can be used to create
## and call upon the matrix and the inverse within other functions (ie. cacheSolve)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function calculates the inverse of a given matrix 'x'.
## The function looks to see if the inverse of x has already been calculated and is
## stored in the environment. The first time you run this function 'm' will be NULL and
## therefore the function will calculate the inverse and store it as the variable 'm'.
## 'm' is then assigned to the setinverse variable which has been created in the enviroment.
## The next time this function is run, m will found in the enviroment and brought back using
## the getinverse function.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  }