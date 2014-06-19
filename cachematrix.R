## R Programming at Coursera, June 2014 (rprog-004)
## Programming Assignment 2: Caching the Inverse of a Matrix

## See usage notes at end of file

## makeCacheMatrix() creates a list of four functions which both establish and
## return the values of a matrix and its inverse.  Once set, the inverse is
## cached as a part of the object and can be retrieved without re-calculating.

makeCacheMatrix <- function(x = matrix() ) {
  
  ## I like to use variable names more meaningful than 'x'
  someMatrix <- x  ## An honest numerical matrix (we assume)
  
  # A brand-new makeCacheMatrix() object will have a missing inverse
  myInverse <- NULL
  
  ## Initiate the value of the matrix
    setMatrix <- function( y ) {
      someMatrix         <<- y
      myInverse <<- NULL  ## Clears the cached inverse (if any)
      
      ## Return a value to indicate the function did something reasonable
      return( invisible( TRUE ) )
    }
    
  ## Provide the current value of the matrix
    getMatrix <- function() {
      return( someMatrix )  ## Simply returns the current matrix value
    }
    
  ## Set the inverse of the matrix to a supplied value
    setInverse <- function( newInverse ) {
      myInverse <<- newInverse
  
      ## Return a value to indicate the function did something reasonable
      return( invisible( TRUE ) )
    }
  
  ## Provide the currently cached inverse of the matrix
    getInverse <- function() {
      return( myInverse )
    }
  
  ## By defining the returned list here, we can modify it (if desired) before
  ## returning it.
  retList <- list( setMatrix  = setMatrix, 
                   getMatrix  = getMatrix, 
                   setInverse = setInverse, 
                   getInverse = getInverse )

  ## Explicit return() statement.
  return( retList )
}


## Use cacheSolve() to invert a special matrix created with makeCacheMatrix. 
## cacheSolve() will use the cached inverse if it exists; if the inverse does
## not exist, cacheSolve() will calculate the inverse and cache it for later
## use.

cacheSolve <- function( x, ...) {
  
  ## Toss in an error check for not-yet-cached input
  if( is.matrix( x ) ) {
    stop( 'x must be a list as returned by makeCacheMatrix()')
  } 
  
  ## I like to use variable names more meaningful than 'x'
  specialMatrix <- x

  ## Return a matrix that is the inverse of 'specialMatrix '.
  ## 'specialMatrix' is really a list of functions such as returned by
  ## makeCacheMatrix( someMatrix )
  
  ## Go get the inverse associated with the input specialMatrix
  myInverse <- specialMatrix$getInverse()
  
  ## If that inverse is not NULL, then we have just retrieved the cached value.
  ## Return that value, and be done.
  if(!is.null( myInverse )) {
    message("Retrieving cached inverse")
    return( myInverse )
  }
  
  ## If we're still in the function, the cached inverse does not exist,
  ## calculate the inverse and cache it
  ## Get the current matrix from the input x
    myMatrix  <- specialMatrix$getMatrix()
  ## Invert the matrix to get the inverse
    myInverse <- solve( myMatrix, ... )
  
  ## Set the newly-calculated inverse for the input x
    specialMatrix$setInverse( myInverse )
  
  ## Send the inverse back to the calling function
    return( myInverse )
}


## Usage notes:

## Set up the initial matrix.  The inverse has not yet been calculated.
#### aMatrix <- makeCacheMatrix( matrix( c( 2, 0, 0, 2 ), nrow= 2 ) )

## You can see the current value of the matrix with
#### aMatrix$getMatrix()

## The matrix values can be changed via
#### aMatrix$setMatrix( matrix( c( 4, 0, 0, 4 ), nrow = 2 ) )
#### aMatrix$getMatrix()  ## Check to see they've changed

## You can see the current value of the inverse with
#### aMatrix$getInverse()
## Which is NULL right now because cacheSolve() hasn't been called since the
## last setMatrix() function

## You can calculate the inverse of the current matrix with
#### ( anInverse <- cacheSolve( aMatrix ) )
## Note: enclose in parentheses to do the assignment and print the value at the same time

## Do it again, and the function tells you it's grabbing cached data
#### ( anInverse <- cacheSolve( aMatrix ) )
