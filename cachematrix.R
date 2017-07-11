

#################################################################################
## This function creates a special "matrix" object that can cache its inverse. ##
#################################################################################

makeCacheMatrix <- function(x = matrix()) {
  
    invMat <- NULL ## invMat set to NULL
    
    #set the value of the Matrix
      setMatrix <- function(y) {
          x <<- y            ## Assigns the input argument to the x object in the parent environment
          invMat <<- NULL ## Assigns the value of NULL to the m object in the parent environment
        }
      
        getMatrix <- function() x                              ## get the value of the Matrix
        setInverse <- function(inverse) invMat <<- inverse  ## set the value of the invertible matrix
        getInverse <- function() invMat                    ## get the value of the invertible matrix
        list(setMatrix = setMatrix, ## assigns each of these functions as an element within a list(), and returns it to the parent environment.
             getMatrix = getMatrix,
             setInverse = setInverse, 
             getInverse = getInverse)
      
}
  



###################################################################################################
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. ##
###################################################################################################


cacheSolve <- function(x, ...) {
    
   
              invMat <- x$getInverse()   #gets the value of the invMat from the makeCacheMatrix function
            if(!is.null(invMat)) {                       ## checks, whether invMat is NULL
                message("Getting cached Inv Matrix")     ## Print message: "Getting cached Inv Matrix"
                return(invMat)                             ## return invert. matrix
              }
              
              ## if value of the invert. matrix == NULL then  
              MatrixData <- x$getMatrix()                 ## get original MatrixData 
              invMat <- solve(MatrixData, ...)             ## use solve to inverse matrix
              x$setInverse(invMat)                         ## set the invert. matrix 
              return(invMat)                               ## return the invertible matrix
              ## Return matrix that is inverse of 'x'
}
  
  
  