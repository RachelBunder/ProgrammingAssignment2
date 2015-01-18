## Created by R. Bunder, 2015-01-18
## Two functions: makeCacheMatrix which returns a list with cached elements for calculating the matrix inverse, and cacheSolve which sets the cached inverse (and returns the inverse)


## This functions takes a matrix, x, and returns a list contatining functions which for getting the inverse (getinverse), setting the inverse (setInverse), getting the matrix (get) and setting a new matrix (set) which additional optional arguments are also passed into
makeCacheMatrix <- function(x = matrix()) 
{
  inverse = NULL
  set <- function(newMatrix){ #when setting a new matrix, set the cached inverse to NULL
                  x <<- newMatrix
                  inverse <<- NULL
  } 
  
  get <- function() x
  
  setInverse <- function(newInverse) newInverse <<- inverse
  
  getInverse <- function() inverse
  
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## This function finds the inverse of a cacheMatrix, X, sets the inverse and returns the inverse. The inverse is calcualted using the solve command
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseCache <- x$getInverse()
  if(!is.null(inverseCache)) #If there is already an inverse calculated, return that
  {
    message("Getting cache data")
    return(inverseCache)
  }
  
  matrix <- x$get()
  inverse <-   solve(matrix, ...) #calculating the inverse
  x$setInverse(inverse)
  
  inverse
}
