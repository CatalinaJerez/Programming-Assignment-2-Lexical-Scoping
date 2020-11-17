
# Assignment: Caching the Inverse of a Matrix

# makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL           # Initialize the inverse property
 
 set <- function(matrix) {  # Function to set the matrix 
  x   <<- matrix
  inv <<- NULL}
 get <- function() x        # Return the matrix
 
 setinverse <- function(inverse) # Function to set inverse of the matrix
  inv <<- inverse
 getinverse <- function() inv    # Return the inverse
 
 # Outputs
 list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)}

 
 
 # CacheSolve
 cacheSolve <- function(s, ...) {
  inverse <- s$getInverse() # Return a matrix that is the inverse of 'x'
  
  if( !is.null(inverse)) { # Just return the inverse if its already set
   message("Getting cached data")
   return(inverse)
  }
  
  data    <- s$get()              # Get the matrix from our object
  inverse <- solve(data) %*% data # Inverse using matrix multiplication
  s$setInverse(x)                 # Set the inverse to the object 
  inverse   }
