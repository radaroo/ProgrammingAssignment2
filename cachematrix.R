######################################################################
## FILE:  cachematrix.R
## Author:  Russ Hammond
## Date:  January 23, 2016
##
## Functions:
##  makeCacheMatrix(matrix)
##      Creates a cache for an input matrix and it's inverse.
##      The function can be invoked through the solveCache function
##      rather than invoking it and it's methods directly.
##      NOTE:  this function assumes the matrix is INVERTIBLE
##
##  cacheSolve(matrix)
##      This function will use the makeCaheMatrix function to
##      cache a matrix, find the inverse and cache the inverse.
##      It's purpose is to improve efficiency when performing 
##      repeated calculations on the same matrix.
######################################################################


######################################################################
# makeCacheMatrix(matrix)
#    Cache an input matrix.  Solve for the inverse and cache the result.
#    Note that this function must be driven by the cacheSolve function.  
######################################################################
makeCacheMatrix <- function(x = matrix()) {

    ############################################################
    # Store new matrix to invert
    ############################################################
    set <- function(y) {
        x_mat <<- y
        inv_mat <<- NULL
    }

    ############################################################
    # Get the stored matrix to be inverted
    ############################################################
    get <- function() {
        if (!exists("x_mat")) return(NULL)
        x_mat
    }
    
    ############################################################
    # Compute the inverse of the stored matrix
    ############################################################
    set_inv <- function() {
        inv_mat <<- solve(x)
    }

    ############################################################
    # Retrieve the inverse of the stored matrix
    ############################################################
    
    get_inv <- function(){
        if (!exists("inv_mat")) return(NULL)
        inv_mat
    }
    
    ############################################################
    # Create list of embedded functions
    ############################################################
    
    list(set=set, 
         get=get, 
         set_inv=set_inv,
         get_inv=get_inv
         )

}


######################################################################
# cacheSolve(matrix)
#    Solves for the inverse of a matrix, and caches the input matrix
#    and resulting inverse.  If the function is called again with an
#    identical matrix, the inverse will be the same as the currently
#    cached inverse, so the cached result is returned rather than
#    recalculating.
######################################################################

cacheSolve <- function(x, ...) {

    #Create the cache matrix object with the input Matrix
    
    y<-makeCacheMatrix(x)
    
    
    #If the input matrix differs from the cached input, or the
    #     inverse of the cached matrix has not yet been calculated
    #     cache the input and compute the inverse
    
    if ((!identical(x, y$get())) | (is.null(y$get_inv()))) {    
#         print("Setting and solving for inv")
         y$set(x)
         y$set_inv()
     }
    
    #Return the inverse of the matrix
    
    y$get_inv()
}
