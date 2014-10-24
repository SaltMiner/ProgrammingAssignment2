#################################################################
## Generate an object that is a matrix with a cachable inverse ##
#################################################################
makeCacheMatrix <- function(x = matrix()) 
{
   i <- NULL
   set <- function(y) {
      x <<- y
      inverse <<- NULL
   }
   get <- function() x
   setinverse <- function(solve) i <<- solve
   getinverse <- function() i
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

###############################################################
## cacheSolve gets the inverse of a 'makeCacheMatrix' object ##
###############################################################
cacheSolve <- function(x, ...) 
{
   ## Return a matrix that is the inverse of 'x'
   i <- x$getinverse()
   if(!is.null(i)) {
      message("getting cached data")
      return(i)
   }
   data <- x$get()
   i <- solve(data, ...)
   x$setinverse(i)
   i
}

############################################################################
## Test function (so I don't have to keep typing into the command prompt) ##
############################################################################
testsolve <- function()
{
   ## Make a simple matrix
   m <- matrix(1:4, nrow=2, ncol=2)
   
   ## Use that matrix to create an object of the new cached type
   m2 = makeCacheMatrix(m)
   
   ## return the inverse of the cached type
   m3 = cacheSolve(m2)

   ## return the inverse of the cached type again
   m4 = cacheSolve(m2)
   
   ## return the inverse of the original matrix
   m5 = solve(m)
   
   if(identical(m3,m5))
   {
      print("gives right answer")
   }
   else
   {
      print("fail case 1")
   }
   
   if(identical(m3,m4))
   {
      print("works twice")
   }
   else
   {
      print("fail case 2")
   }
      
}
