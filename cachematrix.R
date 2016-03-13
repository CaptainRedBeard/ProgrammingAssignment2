## Matrix inversion programming assignment 3/13/16


## Generates a list of the values of a matrix and the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL
        change_mat <- function(y){
        x <<- y
        inv_mat <<- NULL
        }
        main_mat <- function() x
        startInverse <- function(inverse) inv_mat <<- inverse
        returnInverse <- function() inv_mat
        list(change_mat = change_mat, 
                main_mat = main_mat, 
                startInverse = startInverse, 
                returnInverse = returnInverse)
        
}


## Calculates if inverse has been computed and inverts if not already done

cacheSolve <- function(x, ...) {
        inverse <- x$returnInverse()
        if(!is.null(inverse)}{
                message("Collecting cached data.")
                return(inverse)
        }
        matrix_output <- x$main_mat()
        inverse <- solve(matrix_output, ...)
        x$startInverse(inverse)
        inverse
}
