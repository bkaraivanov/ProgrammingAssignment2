## The function "cacheSolve" finds the inverse of a matrix efficiently: any
## time the inverse is computed it is cached, and it is computed only if a 
## valid cached copy is not availbale. The other function, "makeCacheMatrix" 
## implements the technical means by providing methods to set and get the 
## cached matrix and its inverse.

## The function "makeCacheMatrix" creates 4 functions associated with a matrix:
## 1. set(y) sets the cached matrix to y
## 2. get() gets the cached matrix
## 3. setInv(inverse) sets the inverse of the cached matrix to inverse
## 4. getInv() gets the inverse of the cached matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Set the cached matrix.
    set <- function(y) {
        # Set the new matrix.
        x <<- y
        # Clear the old inverse. No inverse is known at this point.
        inv <- NULL
    }
    
    # Get the cached matrix.
    get <- function() x
    
    # Set the inverse computed elsewhere.
    setInv <- function(inverse) {
        inv <<- inverse
    }
    
    # Get the inverse currently kept on record.
    getInv <- function() inv
    
    # Form the return list of four functions.
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## The function "cacheSolve" takes a cached matrix, i.e. a list returned by the
## function "makeCacheMatrix" after it has been set with a specific matrix, and
## use it to deliver the inverse matrix computing it only if a cached copy is 
## not available.
cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        
        # Return the cached inverse.
        return(inv)
    }
    
    # If no inverse is cahced we proceed to compute it:
    # Get the cahced matrix.
    matrix <- x$get()
    
    # Compute the inverse.
    message("computing inverse matrix")
    inv <- solve(matrix, ...)
    
    # Cache the computed inverse.
    x$setInv(inv)
    
    # Return the computed inverse.
    inv
}


## Tests:
# Test with a big matrix. 
mat <- makeCacheMatrix()
# Specify a big diagonal matrix.
mat$set(5 * diag(4000))

# The first call below takes some time to compute the inverse but the remaining
# calls return quickly fo they just grab the cached inverse.
inv <- cacheSolve(mat)
inv <- cacheSolve(mat)
inv <- cacheSolve(mat)
inv <- cacheSolve(mat)
inv <- cacheSolve(mat)

# Set up a small 2x2 matrix.
mat1 <- makeCacheMatrix(rbind(c(2, 1), c(3, 2)))
# Get and print the matrix.
print(mat1$get())

# The first call below computes the inverse but the remaining calls just grab 
# the cached inverse.
print(cacheSolve(mat1))
print(cacheSolve(mat1))
print(cacheSolve(mat1))
print(cacheSolve(mat1))
print(cacheSolve(mat1))
print(cacheSolve(mat1))
