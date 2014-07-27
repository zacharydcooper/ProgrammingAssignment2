##caching the inverse of a matrix
##creating the matrix to cache the inverse
makeCacheMatrix <- function(x = matrix()) {
i <- NULL
#setting the matrix
set <- function(matrix){
        m<<-matrix
        i<<- NULL
}
#getting the matrix that was just set and retrieving it
get<- function() {m}
#setting the inverse of the matrix
setInverse <- function(inverse) {i<<-inverse
}
#returning inverse
getInverse <- function(){i}
}


##creating the inverse of the matrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <-x$getInverse()
        #return the inverse
        if (!is.null(m)){return(m)
        }
        #retrieve the matrix
        data <- x$get()
        #calculate inverse
        m <- solve(data) %*% data
        #set inverse
        x$setInverse(m)
        #return matrix
        m
}
}
