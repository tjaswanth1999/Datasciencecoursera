## these functions will be used to caliculate the inverse of the matrix using cache which will result in the faster caliculation.
## this function will create the matrix and maintains the history of previously caliculated inverses of the matrix
makeCacheMatrix <- function(x = matrix()) 
{
  inv<-NULL
  set<-function(y)
  {
    x<<-y
    inv<<-NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv<<-inverse
  getinverse <- function() inv
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## this function will caliculate the inverse of the matrix if not caliculated and reurns it.
## If inverse of the matrix is already caliculated in the past it'll return it using getinverse() function.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) 
  {
    message("Inverse already caliculated,getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
