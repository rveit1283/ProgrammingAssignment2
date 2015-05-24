## Functions will calculate inverse of a matrix or
## find it in cache. 

## Returns a list of functions to set and get a matrix 
## and set and get an inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        imtx <- NULL
        set <- function(y){
                x <<- y
                imtx <<- NULL
        }
        get <- function() x
        setimtx <- function(invmatrix) imtx <<- invmatrix
        getimtx <- function() imtx
        list(set = set, get = get, 
             setimtx = setimtx,
             getimtx = getimtx)
}


## If inverse already calculated, function will return
## inverse from cache. If not, see if inverse can be 
## calculated. If not, return message describing error.
## Else, calculate inverse and save it to cache. 

cacheSolve <- function(x, ...) {
        mtx <- x$get()
        imtx <- x$getimtx()
        if (!is.null(imtx)){
                message("Getting cached data...")
                return(imtx)
        } else if (nrow(mtx) != ncol(mtx)){
                message("Only square matrices have an inverse.")
        } else if (det(mtx) == 0){
                message("Singular matrices do not have an inverse.")
        } else {
                imtx <- solve(mtx, ...)
                x$setimtx(imtx)
                return(imtx)
        }
}
