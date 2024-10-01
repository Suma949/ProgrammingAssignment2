## makeCacheMatrix consists of set,get,setsolve,getsolve
## library(MASS) is used to calculate inverse for non squared as well as square matrices

library(MASS)
makeCacheMatrix <- function(x = matrix()) 
  {
    s <- NULL             #Initializing inverse as NULL
    set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x         #function to get matrix x
  setsolve <- function(inverse) s <<- inverse
  getsolve <- function()
  {
    inver<-ginv(x)
    inver%*%x             #function to obtain inverse of the matrix
  }
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This is used to get cache data

cacheSolve <- function(x, ...)      ##gets cache data
  {

  s<-x$getsolve()
  if(!is.null(s))           ## checking wheather inverse is NULL
    {
    message("getting inversed matrix")
    return(s)                ##returns inverse value
  }
  data <- x$get()
  s <- solve(data, ...)            #calculates inverse value
  x$setsolve(s)
  s            ## Return a matrix that is the inverse of 'x'
}
