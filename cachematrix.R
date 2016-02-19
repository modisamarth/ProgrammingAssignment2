## Put comments here that give an overall description of what your
## functions do

#matequal is a function to check if two matrices are same
#function to check if two matrices are same 
#we also need to check if their dimensions are same before applyng all
matequal <- function(x, y)
{
    # print(x)
    # print(y)
     (is.matrix(x) && is.matrix(y)) && (dim(x) == dim(y) && all(x == y))
}

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # while making this new type of Cache matrix  method 
    #it will initialize myMatrixInv to NULL 
    myMatrixInv <- NULL
    
    #setMatrix will set the x in the parent environment to new value
    #And if value of the new Matrix is different than the older matrix 
    #it will again make myMatrixInv to null
    setMatrix <- function(modifiedMatrix) 
        {
        if (matequal(x=x,y=modifiedMatrix))
            {
            message("Matrix hasn't changed , no need to compute inverse again")
            }    #matrix equal fuction
 
        else
            {
            x <<- modifiedMatrix
            message("Matrix has changed so inversed will be reset and calculated 
                    on demand")
            myMatrixInv <<- NULL
            }## matrix not equal
        }#set matrix-over 
    
    #simply gets value of x
    getMatrix <- function() x
    
    #setsMatrixInv to a value
    setMatrixInv <- function(matrixInv) 
        {
            myMatrixInv <<- matrixInv
    }
    #simply gets the matrixInv value
    getMatrixInv <- function() myMatrixInv
    
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setMatrixInv = setMatrixInv,
         getMatrixInv = getMatrixInv)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    #first retrive  value of matrixInv
    myMatrixInv <- x$getMatrixInv()
    #check if value is not null 
    #if value is not null implies that inv is already calculated so return the
    #precomputed value 
    if(!is.null(myMatrixInv)) {
        message("getting cached matrix inv value")
        return(myMatrixInv)
    }
    #if value o
    data <- x$getMatrix()
    matrixInv <- solve(data)
    message("calcluating matrixInv ")
    x$setMatrixInv(matrixInv=matrixInv )
    matrixInv
    
}
